#pragma once

#include <arena.h>
#include <minsk-string/string.h>

#include "minsk/meta/concat.h"

#define MINSK_PREFIX_SYNTAX_KIND MINSK_SYNTAX_KIND_

typedef enum
{

#define X(x) MINSK_CONCAT(MINSK_PREFIX_SYNTAX_KIND, x),
#include "minsk/code_analysis/syntax/private/kinds.xmacro"
#undef X
} minsk_syntax_kind_t;

extern string_t
minsk_syntax_kind_display_name(Arena * arena, minsk_syntax_kind_t kind);
