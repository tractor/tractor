// Prototypes for R functions which we are using
int Rf_initialize_R (int ac, char **av);
void Rf_mainloop ();

// Expose function pointers for R callbacks
#define R_INTERFACE_PTRS 1
#include "Rinterface.h"

// Declare the callback pointers which we are using
extern int (*ptr_R_ReadConsole)(const char *, unsigned char *, int, int);
extern void (*ptr_R_WriteConsole)(const char *, int);
extern void (*ptr_R_WriteConsoleEx)(const char *, int, int);
extern void (*ptr_R_CleanUp)(SA_TYPE, int, int);

// Declare R variable which we are using
extern int R_running_as_main_program;

// Pointers to R's standard functions
int (*ptr_R_ReadConsole_default)(const char *, unsigned char *, int, int);
void (*ptr_R_CleanUp_default)(SA_TYPE, int, int);

// Local function prototypes
char * allocate_and_copy_string (const char *from);

void parse_arguments (int argc, const char **argv);

int main (int argc, char **argv);

int read_console (const char *prompt, unsigned char *buffer, int buffer_len, int add_to_history);

void write_console (const char *buffer, int buffer_len, int output_type);

void tidy_up (SA_TYPE save_action, int status, int run_last);
