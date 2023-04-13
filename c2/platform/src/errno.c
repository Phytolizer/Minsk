#include "minsk-platform/errno.h"

#include <stdio.h>
#include <string.h>

#ifdef _WIN32
 #include <windows.h>
#else
#endif

extern void
minsk_report_error(minsk_errno_t error)
{
#ifdef _WIN32
  char * buffer;
  DWORD nchars = FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
    NULL,
    error,
    // default language
    0,
    &buffer,
    1,
    NULL
  );
  assert(nchars != 0);
  (void)fprintf(stderr, "%.*s\n", (int)nchars, buffer);
  LocalFree(buffer);
#else
  (void)fprintf(stderr, "%s\n", strerror(error));
#endif
}
