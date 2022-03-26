#include "styler/styler.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef STYLER_UNIX
#include <unistd.h>
#endif

static styler_control_t *get_control_mode(void) {
  static styler_control_t control_mode = styler_control_auto;
  return &control_mode;
}

static styler_winterm_t *get_winterm_mode(void) {
  static styler_winterm_t winterm_mode = styler_winterm_auto;
  return &winterm_mode;
}

static bool is_support_color(void) {
#ifdef STYLER_UNIX
  const char *const terms[] = {
      "ansi",  "color", "console", "cygwin", "gnome",  "konsole", "kterm",
      "linux", "msys",  "putty",   "rxvt",   "screen", "vt100",   "xterm",
  };

  const char *env_param = getenv("TERM");
  if (env_param == NULL) {
    return false;
  }

  for (size_t i = 0; i < sizeof(terms) / sizeof(const char *); i++) {
    if (strstr(env_param, terms[i]) != NULL) {
      return true;
    }
  }
#else
#endif
  return false;
}

static bool is_terminal(FILE *stream) {
#ifdef STYLER_UNIX
  if (stream == stdout || stream == stderr) {
    return isatty(fileno(stream)) != 0;
  }
#else
#endif
  return false;
}

#ifdef STYLER_WINDOWS
#else
static void set_color(FILE *stream, int value) {
  fprintf(stream, "\x1b[%dm", value);
}
#endif

void styler_apply_style(int style, FILE *stream) {
  styler_control_t control_mode = *get_control_mode();

  if ((control_mode == styler_control_auto && is_support_color() &&
       is_terminal(stream)) ||
      control_mode == styler_control_force) {
    set_color(stream, style);
  }
}

void styler_set_control_mode(styler_control_t mode) {
  *get_control_mode() = mode;
}

void styler_set_winterm_mode(styler_winterm_t mode) {
  *get_winterm_mode() = mode;
}
