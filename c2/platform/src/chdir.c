#include "minsk-platform/chdir.h"

#include <bstrlib.h>

// platform-specific includes
#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#include <unistd.h>
#endif

extern minsk_errno_t minsk_chdir(const_bstring path) {
  char* temp_cstr = bstr2cstr(path, 0);
#ifdef _WIN32
  DWORD ret = SetCurrentDirectory(temp_cstr);
  bcstrfree(temp_cstr);
  if (ret == 0) {
    return GetLastError();
  }
  return MINSK_NO_ERROR;
#else
  int ret = chdir(temp_cstr);
  bcstrfree(temp_cstr);
  if (ret != 0) {
    return errno;
  }
  return MINSK_NO_ERROR;
#endif
}
