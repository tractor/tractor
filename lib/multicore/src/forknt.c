/* Implementation of COW fork() using NTDLL API for Windows systems

   (C)Copyright 2009 Simon Urbanek <simon.urbanek@r-project.org>

   This code is partially based on the book
   "Windows NT/2000 Native API Reference" by Gary Nebbett
   (Sams Publishing, 2000, ISBN 1-57870-199-6)

 */

#ifdef WIN32

#include <windows.h>
#include <setjmp.h>

/* winternl.h is not part of MinGW so we have to declare whatever is needed */

#pragma mark ntdll API types

typedef LONG NTSTATUS;

typedef struct _SYSTEM_HANDLE_INFORMATION {
	ULONG ProcessId;
	UCHAR ObjectTypeNumber;
	UCHAR Flags;
	USHORT Handle;
	PVOID Object;
	ACCESS_MASK GrantedAccess;
} SYSTEM_HANDLE_INFORMATION, *PSYSTEM_HANDLE_INFORMATION;

typedef struct _OBJECT_ATTRIBUTES {
	ULONG Length;
	HANDLE RootDirectory;
	PVOID /* really PUNICODE_STRING */  ObjectName;
	ULONG Attributes;
	PVOID SecurityDescriptor;       /* type SECURITY_DESCRIPTOR */
	PVOID SecurityQualityOfService; /* type SECURITY_QUALITY_OF_SERVICE */
} OBJECT_ATTRIBUTES, *POBJECT_ATTRIBUTES;

typedef enum _MEMORY_INFORMATION_{
	MemoryBasicInformation,
	MemoryWorkingSetList,
	MemorySectionName,
	MemoryBasicVlmInformation
} MEMORY_INFORMATION_CLASS;

typedef struct _CLIENT_ID {
	HANDLE UniqueProcess;
	HANDLE UniqueThread;
} CLIENT_ID, *PCLIENT_ID;

typedef struct _USER_STACK {
	PVOID FixedStackBase;
	PVOID FixedStackLimit;
	PVOID ExpandableStackBase;
	PVOID ExpandableStackLimit;
	PVOID ExpandableStackBottom;
} USER_STACK, *PUSER_STACK;

typedef LONG KPRIORITY;
typedef ULONG_PTR KAFFINITY;
typedef KAFFINITY *PKAFFINITY;

typedef struct _THREAD_BASIC_INFORMATION {
	NTSTATUS                ExitStatus;
	PVOID                   TebBaseAddress;
	CLIENT_ID               ClientId;
	KAFFINITY               AffinityMask;
	KPRIORITY               Priority;
	KPRIORITY               BasePriority;
} THREAD_BASIC_INFORMATION, *PTHREAD_BASIC_INFORMATION;

typedef enum _THREAD_INFORMATION_CLASS {
	ThreadBasicInformation,
	ThreadTimes,
	ThreadPriority,
	ThreadBasePriority,
	ThreadAffinityMask,
	ThreadImpersonationToken,
	ThreadDescriptorTableEntry,
	ThreadEnableAlignmentFaultFixup,
	ThreadEventPair,
	ThreadQuerySetWin32StartAddress,
	ThreadZeroTlsCell,
	ThreadPerformanceCount,
	ThreadAmILastThread,
	ThreadIdealProcessor,
	ThreadPriorityBoost,
	ThreadSetTlsArrayAddress,
	ThreadIsIoPending,
	ThreadHideFromDebugger
} THREAD_INFORMATION_CLASS, *PTHREAD_INFORMATION_CLASS;

typedef enum _SYSTEM_INFORMATION_CLASS { SystemHandleInformation = 0x10 } SYSTEM_INFORMATION_CLASS;

#pragma mark ntdll API - function entry points

typedef NTSTATUS (NTAPI *ZwWriteVirtualMemory_t)(IN HANDLE               ProcessHandle,
												 IN PVOID                BaseAddress,
												 IN PVOID                Buffer,
												 IN ULONG                NumberOfBytesToWrite,
												 OUT PULONG              NumberOfBytesWritten OPTIONAL);
typedef NTSTATUS (NTAPI *ZwCreateProcess_t)(OUT PHANDLE            ProcessHandle, 
											IN  ACCESS_MASK        DesiredAccess, 
											IN  POBJECT_ATTRIBUTES ObjectAttributes, 
											IN  HANDLE             InheriteFromProcessHandle, 
											IN  BOOLEAN            InheritHandles, 
											IN  HANDLE             SectionHandle    OPTIONAL, 
											IN  HANDLE             DebugPort        OPTIONAL, 
											IN  HANDLE             ExceptionPort    OPTIONAL);
typedef NTSTATUS (WINAPI *ZwQuerySystemInformation_t)(SYSTEM_INFORMATION_CLASS SystemInformationClass,
													  PVOID SystemInformation,
													  ULONG SystemInformationLength,
													  PULONG ReturnLength);
typedef NTSTATUS (NTAPI *ZwQueryVirtualMemory_t)(IN  HANDLE ProcessHandle,
												 IN  PVOID BaseAddress,
												 IN  MEMORY_INFORMATION_CLASS MemoryInformationClass,
												 OUT PVOID MemoryInformation,
												 IN  ULONG MemoryInformationLength,
												 OUT PULONG ReturnLength OPTIONAL);
typedef NTSTATUS (NTAPI *ZwGetContextThread_t)(IN HANDLE ThreadHandle, OUT PCONTEXT Context);
typedef NTSTATUS (NTAPI *ZwCreateThread_t)(OUT PHANDLE ThreadHandle,
										   IN  ACCESS_MASK DesiredAccess,
										   IN  POBJECT_ATTRIBUTES ObjectAttributes,
										   IN  HANDLE ProcessHandle,
										   OUT PCLIENT_ID ClientId,
										   IN  PCONTEXT ThreadContext,
										   IN  PUSER_STACK UserStack,
										   IN  BOOLEAN CreateSuspended); 
typedef NTSTATUS (NTAPI *ZwResumeThread_t)(IN HANDLE ThreadHandle, OUT PULONG SuspendCount OPTIONAL);
typedef NTSTATUS (NTAPI *ZwClose_t)(IN HANDLE ObjectHandle);
typedef NTSTATUS (NTAPI *ZwQueryInformationThread_t)(IN HANDLE               ThreadHandle,
													 IN THREAD_INFORMATION_CLASS ThreadInformationClass,
													 OUT PVOID               ThreadInformation,
													 IN ULONG                ThreadInformationLength,
													 OUT PULONG              ReturnLength OPTIONAL );

/* function pointers */
static ZwCreateProcess_t ZwCreateProcess;
static ZwQuerySystemInformation_t ZwQuerySystemInformation;
static ZwQueryVirtualMemory_t ZwQueryVirtualMemory;
static ZwCreateThread_t ZwCreateThread;
static ZwGetContextThread_t ZwGetContextThread;
static ZwResumeThread_t ZwResumeThread;
static ZwClose_t ZwClose;
static ZwQueryInformationThread_t ZwQueryInformationThread;
static ZwWriteVirtualMemory_t ZwWriteVirtualMemory;

/* macro definitions */

#define NtCurrentProcess() ((HANDLE)-1)
#define NtCurrentThread() ((HANDLE) -2)
/* we use really the Nt versions - so the following is just for completeness */
#define ZwCurrentProcess() NtCurrentProcess()     
#define ZwCurrentThread() NtCurrentThread()

#define STATUS_INFO_LENGTH_MISMATCH      ((NTSTATUS)0xC0000004L)
#define STATUS_SUCCESS ((NTSTATUS)0x00000000L)

#pragma mark -- helper functions --

#ifdef INHERIT_ALL
/* set all handles belonging to this process as inheritable */
static void set_inherit_all()
{
	ULONG n = 0x1000;
	PULONG p = (PULONG) calloc(n, sizeof(ULONG));

	/* some guesswork to allocate a structure that will fit it all */
	while (ZwQuerySystemInformation(SystemHandleInformation, p, n * sizeof(ULONG), 0) == STATUS_INFO_LENGTH_MISMATCH) {
		free(p);
		n *= 2;
		p = (PULONG) calloc(n, sizeof(ULONG));
	}
	
	/* p points to an ULONG with the count, the entries follow (hence p[0] is the size and p[1] is where the first entry starts */
	PSYSTEM_HANDLE_INFORMATION h = (PSYSTEM_HANDLE_INFORMATION)(p + 1);

	ULONG pid = GetCurrentProcessId();
	ULONG i = 0, count = *p;

	while (i < count) {
		if (h[i].ProcessId == pid)
			SetHandleInformation((HANDLE)(ULONG) h[i].Handle, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
		i++;
	}
	free(p);
}
#endif

/* setjmp env for the jump back into the fork() function */
static jmp_buf jenv;

/* entry point for our child thread process - just longjmp into fork */
static int child_entry(void) {
	longjmp(jenv, 1);
	return 0;
}

/* initialize NTDLL entry points */
static int init_NTAPI(void) {
	HANDLE ntdll = GetModuleHandle("ntdll");
	if (ntdll == NULL) return -1;
	ZwCreateProcess = (ZwCreateProcess_t) GetProcAddress(ntdll, "ZwCreateProcess");
	ZwQuerySystemInformation = (ZwQuerySystemInformation_t) GetProcAddress(ntdll, "ZwQuerySystemInformation");
	ZwQueryVirtualMemory = (ZwQueryVirtualMemory_t) GetProcAddress(ntdll, "ZwQueryVirtualMemory");
	ZwCreateThread = (ZwCreateThread_t) GetProcAddress(ntdll, "ZwCreateThread");
	ZwGetContextThread = (ZwGetContextThread_t) GetProcAddress(ntdll, "ZwGetContextThread");
	ZwResumeThread = (ZwResumeThread_t) GetProcAddress(ntdll, "ZwResumeThread");
	ZwQueryInformationThread = (ZwQueryInformationThread_t) GetProcAddress(ntdll, "ZwQueryInformationThread");
	ZwWriteVirtualMemory = (ZwWriteVirtualMemory_t) GetProcAddress(ntdll, "ZwWriteVirtualMemory");
	ZwClose = (ZwClose_t) GetProcAddress(ntdll, "ZwClose");
	/* in theory we chould check all of them - but I guess that would be a waste of time ... */
	return (!ZwCreateProcess) ? -1 : 0;
}

#pragma mark -- fork() --

int fork(void) {
	if (setjmp(jenv) != 0) return 0; /* return as a child */

	/* check whether the entry points are initilized and get them if necessary */
	if (!ZwCreateProcess && init_NTAPI()) return -1;

#ifdef INHERIT_ALL
	/* make sure all handles are inheritable */
	set_inherit_all();
#endif

	HANDLE hProcess = 0, hThread = 0;
	OBJECT_ATTRIBUTES oa = { sizeof(oa) };

	/* create forked process */
	ZwCreateProcess(&hProcess, PROCESS_ALL_ACCESS, &oa, NtCurrentProcess(), TRUE, 0, 0, 0);

	CONTEXT context = {CONTEXT_FULL | CONTEXT_DEBUG_REGISTERS | CONTEXT_FLOATING_POINT};

	/* set the Eip for the child process to our child function */
	ZwGetContextThread(NtCurrentThread(), &context);
	context.Eip = (ULONG)child_entry;

	MEMORY_BASIC_INFORMATION mbi;
	ZwQueryVirtualMemory(NtCurrentProcess(), (PVOID)context.Esp, MemoryBasicInformation, &mbi, sizeof mbi, 0);

	USER_STACK stack = {0, 0, (PCHAR)mbi.BaseAddress + mbi.RegionSize, mbi.BaseAddress, mbi.AllocationBase};
	CLIENT_ID cid;

	/* create thread using the modified context and stack */
	ZwCreateThread(&hThread, THREAD_ALL_ACCESS, &oa, hProcess, &cid, &context, &stack, TRUE);

	/* copy exception table */
	THREAD_BASIC_INFORMATION tbi;
	ZwQueryInformationThread(NtCurrentThread(), ThreadBasicInformation, &tbi, sizeof tbi, 0);
	PNT_TIB tib = (PNT_TIB)tbi.TebBaseAddress;
	ZwQueryInformationThread(hThread, ThreadBasicInformation, &tbi, sizeof tbi, 0);
	ZwWriteVirtualMemory(hProcess, tbi.TebBaseAddress, &tib->ExceptionList, sizeof tib->ExceptionList, 0);

	/* start (resume really) the child */
	ZwResumeThread(hThread, 0);

	/* clean up */
	ZwClose(hThread);
	ZwClose(hProcess);

	/* exit with child's pid */
	return (int)cid.UniqueProcess;
}


/* Dear Emacs, please be nice and use
   Local Variables:
   mode:c
   tab-width: 4
   c-basic-offset:4
   End:
*/

#else
/* unix has fork() already */
#include <unistd.h>
#endif
