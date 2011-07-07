#include <string.h>
#include <fcntl.h> /* for _O_BINARY */

/* our implementation from windows/forknt.c */
extern int fork(); 

/* our own implementation of select using WaitForMultipleObjects */
int pipe_select(int xfd, fd_set *sr, fd_set *sw, fd_set *se, struct timeval *timeout);

#define sleep(X) Sleep((X) * 1000)
#define pipe(fds) _pipe(fds, 4096, _O_BINARY)

/* Windows includes re-define ERROR which is used by R .. */
#ifdef ERROR
#undef ERROR
#endif

