#include "minsk/code_analysis/syntax/kind.h"

#include <arena.h>
#include <ctype.h>
#include <stdbool.h>

#include "minsk-platform/debugger.h"

static string_t prettify(Arena* arena, string_t str)
{
  string_t result = EMPTY_STRING;
  string_reserve_arena(arena, &result, str.length);

  bool next_upper = true;
  for (size_t i = 0; i < str.length; i++)
  {
    if (str.data[i] == '_')
    {
      next_upper = true;
    }
    else if (next_upper)
    {
      next_upper = false;
      string_append_arena(arena, &result, STRING_REF_DATA(&str.data[i], 1));
    }
    else
    {
      char c = tolower(str.data[i]);
      string_append_arena(arena, &result, STRING_REF_DATA(&c, 1));
    }
  }

  return result;
}

extern string_t
minsk_syntax_kind_display_name(Arena* arena, minsk_syntax_kind_t kind)
{
  switch (kind)
  {
#define X(x)                                \
  case MINSK_SYNTAX_KIND_##x:               \
  {                                         \
    return prettify(arena, STRING_REF(#x)); \
  }
#include "minsk/code_analysis/syntax/private/kinds.xmacro"
#undef X
  }

  DEBUGGER_FATAL("invalid syntax kind %d", kind);
}
