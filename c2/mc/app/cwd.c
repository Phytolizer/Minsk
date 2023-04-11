#include "mc/cwd.h"

#include <minsk-platform/chdir.h>
#include <minsk-platform/errno.h>
#include <stdlib.h>

extern bool mc_set_cwd_from_meson(void) {
  const char* srcdir = getenv("MESON_SOURCE_ROOT");
  if (srcdir != NULL) {
    struct tagbstring str;
    cstr2tbstr(str, srcdir);
    minsk_errno_t error = minsk_chdir(&str);
    if (error != MINSK_NO_ERROR) {
      minsk_report_error(error);
      return false;
    }
  }
  return true;
}
