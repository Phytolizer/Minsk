#include "minsk/code_analysis/syntax/kind.h"

#include <arena.h>
#include <minsk-platform/debugger.h>
#include <minsk-string/string.h>

#include "minsk/meta/snake_to_camel.h"

extern string_t
minsk_syntax_kind_display_name(Arena * arena, minsk_syntax_kind_t kind)
{
  switch (kind)
  {
#define X(x)                                    \
 case MINSK_SYNTAX_KIND_##x:                    \
 {                                              \
  return snake_to_camel(arena, STRING_REF(#x)); \
 }
#include "minsk/code_analysis/syntax/private/kinds.xmacro"
#undef X
    default: break;
  }

  DEBUGGER_FATAL("invalid syntax kind %d", kind);
}
