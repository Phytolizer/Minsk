#include "minsk-platform/chdir.h"

#include <minsk-string/string.h>

// platform-specific includes
#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#include <unistd.h>
#endif

extern minsk_errno_t minsk_chdir(string_t path)
{
  string_t temp = string_term(path, 0);
#ifdef _WIN32
  DWORD ret = SetCurrentDirectory(temp.data);
  string_free(temp);
  if (ret == 0)
  {
    return GetLastError();
  }
  return MINSK_NO_ERROR;
#else
  int ret = chdir(temp.data);
  string_free(temp);
  if (ret != 0)
  {
    return errno;
  }
  return MINSK_NO_ERROR;
#endif
}
