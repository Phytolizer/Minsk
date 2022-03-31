//! Adapted from
//! https://github.com/utilForever/styler/blob/main/include/styler.hpp

#pragma once

#include <stdbool.h>
#include <stdio.h>

#if defined(__unix__) || defined(__unix) || defined(__linux__) ||              \
    defined(__APPLE__) || defined(__MACH__)
#define STYLER_UNIX
#elif defined(WIN32) || defined(_WIN32) || defined(_WIN64)
#define STYLER_WINDOWS
#endif

#define STYLER_STYLES_X                                                        \
  X(reset)                                                                     \
  X(bold)                                                                      \
  X(faint)                                                                     \
  X(italic)                                                                    \
  X(underline)                                                                 \
  X(slow_blink)                                                                \
  X(rapid_blink)                                                               \
  X(reverse)                                                                   \
  X(conceal)                                                                   \
  X(crossed)

typedef enum {
#define X(x) styler_style_##x,
  STYLER_STYLES_X
#undef X
} styler_style_t;

#define STYLER_FGS_X                                                           \
  X(black, 30)                                                                 \
  X(red, 31)                                                                   \
  X(green, 32)                                                                 \
  X(yellow, 33)                                                                \
  X(blue, 34)                                                                  \
  X(magenta, 35)                                                               \
  X(cyan, 36)                                                                  \
  X(white, 37)                                                                 \
  X(reset, 39)

typedef enum {
#define X(x, y) styler_fg_##x = (y),
  STYLER_FGS_X
#undef X
} styler_fg_t;

#define STYLER_BGS_X                                                           \
  X(black, 40)                                                                 \
  X(red, 41)                                                                   \
  X(green, 42)                                                                 \
  X(yellow, 43)                                                                \
  X(blue, 44)                                                                  \
  X(magenta, 45)                                                               \
  X(cyan, 46)                                                                  \
  X(white, 47)                                                                 \
  X(reset, 49)

typedef enum {
#define X(x, y) styler_bg_##x = (y),
  STYLER_BGS_X
#undef X
} styler_bg_t;

#define STYLER_BRIGHT_FGS_X                                                    \
  X(black, 90)                                                                 \
  X(red, 91)                                                                   \
  X(green, 92)                                                                 \
  X(yellow, 93)                                                                \
  X(blue, 94)                                                                  \
  X(magenta, 95)                                                               \
  X(cyan, 96)                                                                  \
  X(white, 97)

typedef enum {
#define X(x, y) styler_fg_bright_##x = (y),
  STYLER_BRIGHT_FGS_X
#undef X
} styler_fg_bright_t;

#define STYLER_BRIGHT_BGS_X                                                    \
  X(black, 100)                                                                \
  X(red, 101)                                                                  \
  X(green, 102)                                                                \
  X(yellow, 103)                                                               \
  X(blue, 104)                                                                 \
  X(magenta, 105)                                                              \
  X(cyan, 106)                                                                 \
  X(white, 107)

typedef enum {
#define X(x, y) styler_bg_bright_##x = (y),
  STYLER_BRIGHT_BGS_X
#undef X
} styler_bg_bright_t;

#define STYLER_CONTROLS_X                                                      \
  X(off)                                                                       \
  X(auto)                                                                      \
  X(force)

typedef enum {
#define X(x) styler_control_##x,
  STYLER_CONTROLS_X
#undef X
} styler_control_t;

#define STYLER_WINTERMS_X                                                      \
  X(auto)                                                                      \
  X(ansi)                                                                      \
  X(native)

typedef enum {
#define X(x) styler_winterm_##x,
  STYLER_WINTERMS_X
#undef X
} styler_winterm_t;

void styler_apply_fg(styler_fg_t fg, FILE* stream);
void styler_apply_bg(styler_bg_t bg, FILE* stream);
void styler_apply_fg_bright(styler_fg_bright_t fg, FILE* stream);
void styler_apply_bg_bright(styler_bg_bright_t bg, FILE* stream);
void styler_apply_style(styler_style_t style, FILE* stream);
void styler_set_control_mode(styler_control_t mode);
void styler_set_winterm_mode(styler_winterm_t mode);
