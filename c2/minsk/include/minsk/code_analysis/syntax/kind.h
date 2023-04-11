#pragma once

#include <bstrlib.h>

#include "minsk/meta/concat.h"

#define MINSK_PREFIX_SYNTAX_KIND MINSK_SYNTAX_KIND_

typedef enum
{
#define X(x) MINSK_CONCAT(MINSK_PREFIX_SYNTAX_KIND, x),
#include "./private/kinds.xmacro"
#undef X
} minsk_syntax_kind_t;

extern bstring minsk_syntax_kind_display_name(minsk_syntax_kind_t kind);
