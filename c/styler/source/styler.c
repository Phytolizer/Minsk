#include "styler/styler.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef STYLER_UNIX
#include <unistd.h>
#else
#include <io.h>
#include <wchar.h>
#include <windows.h>
#endif

static styler_control_t* get_control_mode(void) {
  static styler_control_t control_mode = styler_control_auto;
  return &control_mode;
}

static styler_winterm_t* get_winterm_mode(void) {
  static styler_winterm_t winterm_mode = styler_winterm_auto;
  return &winterm_mode;
}

static bool is_support_color(void) {
#ifdef STYLER_UNIX
  const char* const terms[] = {
      "ansi",
      "color",
      "console",
      "cygwin",
      "gnome",
      "konsole",
      "kterm",
      "linux",
      "msys",
      "putty",
      "rxvt",
      "screen",
      "vt100",
      "xterm",
  };

  const char* env_param = getenv("TERM");
  if (env_param == NULL) {
    return false;
  }

  for (size_t i = 0; i < sizeof(terms) / sizeof(const char*); i++) {
    if (strstr(env_param, terms[i]) != NULL) {
      return true;
    }
  }

  return false;
#else
  return true;
#endif
}

#ifdef STYLER_WINDOWS
typedef BOOL (*get_file_information_by_handle_ex_func_t)(HANDLE hFile,
    FILE_INFO_BY_HANDLE_CLASS FileInformationClass, LPVOID lpFileInformation,
    DWORD dwBufferSize);

static bool is_msys_pty(int fd) {
  get_file_information_by_handle_ex_func_t ptr_file_info =
      (get_file_information_by_handle_ex_func_t)GetProcAddress(
          GetModuleHandle(TEXT("kernel32.dll")),
          "GetFileInformationByHandleEx");

  if (ptr_file_info == NULL) {
    return false;
  }

  HANDLE handle = (HANDLE)_get_osfhandle(fd);
  if (handle == INVALID_HANDLE_VALUE) {
    return false;
  }

  if (GetFileType(handle) != FILE_TYPE_PIPE) {
    return false;
  }

  typedef struct {
    DWORD file_name_length;
    WCHAR file_name[MAX_PATH];
  } my_file_name_info_t;

  my_file_name_info_t* p_name_info = malloc(sizeof(my_file_name_info_t));
  if (p_name_info == NULL) {
    return false;
  }

  if (!ptr_file_info(
          handle, FileNameInfo, p_name_info, sizeof(my_file_name_info_t))) {
    return false;
  }

  if ((wcsstr(p_name_info->file_name, L"msys-") == NULL &&
          wcsstr(p_name_info->file_name, L"cygwin-") == NULL) ||
      wcsstr(p_name_info->file_name, L"-pty") == NULL) {
    free(p_name_info);
    return false;
  }

  free(p_name_info);
  return true;
}
#endif

static bool is_terminal(FILE* stream) {
#ifdef STYLER_UNIX
  if (stream == stdout || stream == stderr) {
    return isatty(fileno(stream)) != 0;
  }
#else
  if (stream == stdout) {
    static bool init = false;
    static bool stdout_term;
    if (!init) {
      init = true;
      stdout_term = _isatty(_fileno(stdout)) || is_msys_pty(_fileno(stdout));
    }
    return stdout_term;
  }
  if (stream == stderr) {
    static bool init = false;
    static bool stderr_term;
    if (!init) {
      init = true;
      stderr_term = _isatty(_fileno(stdout)) || is_msys_pty(_fileno(stdout));
    }
    return stderr_term;
  }
#endif
  return false;
}

#ifdef STYLER_WINDOWS
typedef struct {
  BYTE foreground_color;
  BYTE background_color;
  BYTE bold;
  BYTE underline;
  BOOLEAN reverse;
  BOOLEAN conceal;
} sgr_t;

#define ATTR_COLORS_X                                                          \
  X(black, 0)                                                                  \
  X(red, 4)                                                                    \
  X(green, 2)                                                                  \
  X(yellow, 6)                                                                 \
  X(blue, 1)                                                                   \
  X(magenta, 5)                                                                \
  X(cyan, 3)                                                                   \
  X(white, 7)

typedef enum {
#define X(x, y) attr_color_##x,
  ATTR_COLORS_X
#undef X
} attr_color_t;

static HANDLE get_console_handle(FILE* stream) {
  if (stream == stdout) {
    return GetStdHandle(STD_OUTPUT_HANDLE);
  }

  if (stream == stderr) {
    return GetStdHandle(STD_ERROR_HANDLE);
  }

  return INVALID_HANDLE_VALUE;
}

static bool set_winterm_ansi_colors(FILE* stream) {
  HANDLE handle = get_console_handle(stream);
  if (handle == INVALID_HANDLE_VALUE) {
    return false;
  }

  DWORD mode = 0;
  if (!GetConsoleMode(handle, &mode)) {
    return false;
  }

  mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if (!SetConsoleMode(handle, mode)) {
    return false;
  }

  return true;
}

static bool is_support_ansi(FILE* stream) {
  if (stream == stdout) {
    static bool init = false;
    static bool stdout_ansi;
    if (!init) {
      init = true;
      stdout_ansi =
          is_msys_pty(_fileno(stdout)) || set_winterm_ansi_colors(stream);
    }
    return stdout_ansi;
  }

  if (stream == stderr) {
    static bool init = false;
    static bool stderr_ansi;
    if (!init) {
      init = true;
      stderr_ansi =
          is_msys_pty(_fileno(stderr)) || set_winterm_ansi_colors(stream);
    }
    return stderr_ansi;
  }

  return false;
}

static const sgr_t* get_default_state(void) {
  static bool init = false;
  static sgr_t default_sgr;

  if (!init) {
    init = true;
    CONSOLE_SCREEN_BUFFER_INFO info;
    WORD attrib = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;

    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info) ||
        GetConsoleScreenBufferInfo(GetStdHandle(STD_ERROR_HANDLE), &info)) {
      attrib = info.wAttributes;
    }

    sgr_t sgr = {0};
    sgr.foreground_color = attrib & 0x0F;
    sgr.background_color = (attrib & 0xF0) >> 4;
    default_sgr = sgr;
  }

  return &default_sgr;
}

static sgr_t* get_current_state(void) {
  static bool init = false;
  static sgr_t sgr;
  if (!init) {
    init = true;
    sgr = *get_default_state();
  }
  return &sgr;
}

static BYTE ansi_2_attr(BYTE rgb) {
  static const attr_color_t rev[] = {
      attr_color_black,
      attr_color_red,
      attr_color_green,
      attr_color_yellow,
      attr_color_blue,
      attr_color_magenta,
      attr_color_cyan,
      attr_color_white,
  };
  return (BYTE)rev[rgb];
}

static WORD sgr_2_attr(const sgr_t* state) {
  WORD attr;

  if (state->conceal) {
    if (state->reverse) {
      attr = (BYTE)(state->foreground_color << 4) | state->foreground_color;
      if (state->bold) {
        attr |= FOREGROUND_INTENSITY | BACKGROUND_INTENSITY;
      }
    } else {
      attr = (BYTE)(state->background_color << 4) | state->background_color;
      if (state->underline) {
        attr |= FOREGROUND_INTENSITY | BACKGROUND_INTENSITY;
      }
    }
  } else if (state->reverse) {
    attr = (BYTE)(state->foreground_color << 4) | state->background_color;
    if (state->bold) {
      attr |= BACKGROUND_INTENSITY;
    }
    if (state->underline) {
      attr |= FOREGROUND_INTENSITY;
    }
  } else {
    attr = state->foreground_color | (BYTE)(state->background_color << 4) |
        state->bold | state->underline;
  }

  return attr;
}

static void set_win_sgr_style(styler_style_t style, sgr_t* state) {
  switch (style) {
  case styler_style_reset:
    *state = *get_default_state();
    break;
  case styler_style_bold:
    state->bold = FOREGROUND_INTENSITY;
    break;
  case styler_style_underline:
  case styler_style_slow_blink:
    state->underline = BACKGROUND_INTENSITY;
    break;
  case styler_style_reverse:
    state->reverse = true;
    break;
  case styler_style_conceal:
    state->conceal = true;
    break;
  default:
    break;
  }
}

static void set_win_sgr_fg(styler_fg_t fg, sgr_t* state) {
  if (fg != styler_fg_reset) {
    state->foreground_color = ansi_2_attr((BYTE)fg - 30);
  } else {
    state->foreground_color = get_default_state()->foreground_color;
  }
}

static void set_win_sgr_bg(styler_bg_t bg, sgr_t* state) {
  if (bg != styler_bg_reset) {
    state->background_color = ansi_2_attr((BYTE)bg - 40);
  } else {
    state->background_color = get_default_state()->background_color;
  }
}

static void set_win_sgr_fg_bright(styler_fg_bright_t fg, sgr_t* state) {
  state->foreground_color =
      (BACKGROUND_INTENSITY >> 4) | ansi_2_attr((BYTE)fg - 90);
}

static void set_win_sgr_bg_bright(styler_bg_bright_t bg, sgr_t* state) {
  state->background_color = FOREGROUND_INTENSITY | ansi_2_attr((BYTE)bg - 100);
}

static void set_win_color_ansi(FILE* stream, int value) {
  fprintf(stream, "\x1b[%dm", value);
}

static void set_win_color_native_fg(FILE* stream, styler_fg_t fg) {
  HANDLE handle = get_console_handle(stream);
  if (handle != INVALID_HANDLE_VALUE) {
    set_win_sgr_fg(fg, get_current_state());
    fflush(stream);
    SetConsoleTextAttribute(handle, sgr_2_attr(get_current_state()));
  }
}

static void set_win_color_native_bg(FILE* stream, styler_bg_t bg) {
  HANDLE handle = get_console_handle(stream);
  if (handle != INVALID_HANDLE_VALUE) {
    set_win_sgr_bg(bg, get_current_state());
    fflush(stream);
    SetConsoleTextAttribute(handle, sgr_2_attr(get_current_state()));
  }
}

static void set_win_color_native_fg_bright(
    FILE* stream, styler_fg_bright_t fg) {
  HANDLE handle = get_console_handle(stream);
  if (handle != INVALID_HANDLE_VALUE) {
    set_win_sgr_fg_bright(fg, get_current_state());
    fflush(stream);
    SetConsoleTextAttribute(handle, sgr_2_attr(get_current_state()));
  }
}

static void set_win_color_native_bg_bright(
    FILE* stream, styler_bg_bright_t bg) {
  HANDLE handle = get_console_handle(stream);
  if (handle != INVALID_HANDLE_VALUE) {
    set_win_sgr_bg_bright(bg, get_current_state());
    fflush(stream);
    SetConsoleTextAttribute(handle, sgr_2_attr(get_current_state()));
  }
}

static void set_win_color_native_style(FILE* stream, styler_style_t style) {
  HANDLE handle = get_console_handle(stream);
  if (handle != INVALID_HANDLE_VALUE) {
    set_win_sgr_style(style, get_current_state());
    fflush(stream);
    SetConsoleTextAttribute(handle, sgr_2_attr(get_current_state()));
  }
}

static void set_color_fg(FILE* stream, styler_fg_t fg) {
  if (*get_winterm_mode() == styler_winterm_auto) {
    if (is_support_ansi(stream)) {
      set_win_color_ansi(stream, fg);
    } else {
      set_win_color_native_fg(stream, fg);
    }
  } else if (*get_winterm_mode() == styler_winterm_ansi) {
    set_win_color_ansi(stream, fg);
  } else {
    set_win_color_native_fg(stream, fg);
  }
}

static void set_color_bg(FILE* stream, styler_bg_t bg) {
  if (*get_winterm_mode() == styler_winterm_auto) {
    if (is_support_ansi(stream)) {
      set_win_color_ansi(stream, bg);
    } else {
      set_win_color_native_bg(stream, bg);
    }
  } else if (*get_winterm_mode() == styler_winterm_ansi) {
    set_win_color_ansi(stream, bg);
  } else {
    set_win_color_native_bg(stream, bg);
  }
}

static void set_color_fg_bright(FILE* stream, styler_fg_bright_t fg) {
  if (*get_winterm_mode() == styler_winterm_auto) {
    if (is_support_ansi(stream)) {
      set_win_color_ansi(stream, fg);
    } else {
      set_win_color_native_fg_bright(stream, fg);
    }
  } else if (*get_winterm_mode() == styler_winterm_ansi) {
    set_win_color_ansi(stream, fg);
  } else {
    set_win_color_native_fg_bright(stream, fg);
  }
}

static void set_color_bg_bright(FILE* stream, styler_bg_bright_t bg) {
  if (*get_winterm_mode() == styler_winterm_auto) {
    if (is_support_ansi(stream)) {
      set_win_color_ansi(stream, bg);
    } else {
      set_win_color_native_bg_bright(stream, bg);
    }
  } else if (*get_winterm_mode() == styler_winterm_ansi) {
    set_win_color_ansi(stream, bg);
  } else {
    set_win_color_native_bg_bright(stream, bg);
  }
}

static void set_color_style(FILE* stream, styler_style_t style) {
  if (*get_winterm_mode() == styler_winterm_auto) {
    if (is_support_ansi(stream)) {
      set_win_color_ansi(stream, style);
    } else {
      set_win_color_native_style(stream, style);
    }
  } else if (*get_winterm_mode() == styler_winterm_ansi) {
    set_win_color_ansi(stream, style);
  } else {
    set_win_color_native_style(stream, style);
  }
}
#else
static void set_color(FILE* stream, int value) {
  fprintf(stream, "\x1b[%dm", value);
}

static void set_color_fg(FILE* stream, int value) { set_color(stream, value); }

static void set_color_bg(FILE* stream, int value) { set_color(stream, value); }

static void set_color_fg_bright(FILE* stream, int value) {
  set_color(stream, value);
}

static void set_color_bg_bright(FILE* stream, int value) {
  set_color(stream, value);
}

static void set_color_style(FILE* stream, int value) {
  set_color(stream, value);
}
#endif

void styler_apply_fg(styler_fg_t fg, FILE* stream) {
  styler_control_t control_mode = *get_control_mode();

  if ((control_mode == styler_control_auto && is_support_color() &&
          is_terminal(stream)) ||
      control_mode == styler_control_force) {
    set_color_fg(stream, fg);
  }
}

void styler_apply_bg(styler_bg_t bg, FILE* stream) {
  styler_control_t control_mode = *get_control_mode();

  if ((control_mode == styler_control_auto && is_support_color() &&
          is_terminal(stream)) ||
      control_mode == styler_control_force) {
    set_color_bg(stream, bg);
  }
}

void styler_apply_fg_bright(styler_fg_bright_t fg, FILE* stream) {
  styler_control_t control_mode = *get_control_mode();

  if ((control_mode == styler_control_auto && is_support_color() &&
          is_terminal(stream)) ||
      control_mode == styler_control_force) {
    set_color_fg_bright(stream, fg);
  }
}

void styler_apply_bg_bright(styler_bg_bright_t bg, FILE* stream) {
  styler_control_t control_mode = *get_control_mode();

  if ((control_mode == styler_control_auto && is_support_color() &&
          is_terminal(stream)) ||
      control_mode == styler_control_force) {
    set_color_bg_bright(stream, bg);
  }
}

void styler_apply_style(styler_style_t style, FILE* stream) {
  styler_control_t control_mode = *get_control_mode();

  if ((control_mode == styler_control_auto && is_support_color() &&
          is_terminal(stream)) ||
      control_mode == styler_control_force) {
    set_color_style(stream, style);
  }
}

void styler_set_control_mode(styler_control_t mode) {
  *get_control_mode() = mode;
}

void styler_set_winterm_mode(styler_winterm_t mode) {
  *get_winterm_mode() = mode;
}
