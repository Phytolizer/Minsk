#pragma once

#define MINSK_PREFIX_SYNTAX_KIND MINSK_SYNTAX_KIND

typedef enum
{
#define X(x) MINSK_PREFIX_SYNTAX_KIND##_##x,
#include "./private/kinds.xmacro"
#undef X
} minsk_syntax_kind_t;
