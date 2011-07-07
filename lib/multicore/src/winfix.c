/* work-arounds and fixes for Windows */

#ifdef WIN32

#include <windows.h>
#include "winfix.h"

/* Wait for any of the descriptors to become signalled. The implementation uses WaitForMultipleObjects which can only signal one object at a time, so in fact the result will be -1, 0 or 1. Also note that we are using readfds *only*, all others are ignored. */
int pipe_select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *errorfds, struct timeval *timeout)
{
	HANDLE h[MAXIMUM_WAIT_OBJECTS];
	int fd[MAXIMUM_WAIT_OBJECTS];
	DWORD n, hs = 0, tout = INFINITE;
	int i = 0, sii = -1;

	while (i < nfds) {
		if (FD_ISSET(i, readfds)) {
			h[hs] = (HANDLE) _get_osfhandle(i);
			fd[hs++] = i;
			if (hs >= MAXIMUM_WAIT_OBJECTS) break;
		}
		i++;
	}
	if (hs < 1) return -1;
	if (timeout) tout =  (timeout->tv_sec * 1000) + (timeout->tv_usec / 1000);
	n = WaitForMultipleObjects(hs, h, FALSE, tout);
	if (n >= WAIT_OBJECT_0 && n - WAIT_OBJECT_0 < hs) sii = (int) (n - WAIT_OBJECT_0);
	else if (n >= WAIT_ABANDONED_0 && n - WAIT_ABANDONED_0 < hs) sii = (int) (n - WAIT_ABANDONED_0);
	if (n == WAIT_FAILED) return -1;
	if (sii == -1) return 0;
	else {
		FD_ZERO(readfds);
		FD_SET(fd[n - WAIT_OBJECT_0], readfds);
	}
	return 1;
}
#else
/* this is only a dummy to avoid warning by various compilers about empty files, superfluous semicolons etc. */
typedef int foo_t;
#endif
