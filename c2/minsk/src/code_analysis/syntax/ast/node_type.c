#include "minsk/code_analysis/syntax/ast/node_type.h"

#include <arena.h>
#include <minsk-platform/debugger.h>
#include <minsk-string/string.h>

#include "minsk/meta/snake_to_camel.h"

extern string_t
minsk_syntax_node_type_display_name(
  Arena * arena,
  minsk_syntax_node_type_t type
)
{
  switch (type)
  {
#define X(x) \
  case MINSK_CONCAT(MINSK_PREFIX_SYNTAX_NODE, x): \
    return snake_to_camel(arena, STRING_REF(#x));
#include "minsk/code_analysis/syntax/ast/private/node_types.xmacro"
#undef X
  }

  DEBUGGER_FATAL("bad syntax node type %d", type);
}
