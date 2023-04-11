#include "minsk/code_analysis/syntax/kind.h"

#include <stdbool.h>

#include "minsk-platform/debugger.h"

static bstring prettify(const_bstring str)
{
  bstring result = blk2bstr("", 0);
  ballocmin(result, blength(str));

  bool next_upper = true;
  for (size_t i = 0;; i++)
  {
    char c = bchar(str, i);
    if (c == '\0')
    {
      break;
    }
    else if (c == '_')
    {
      next_upper = true;
    }
    else if (next_upper)
    {
      next_upper = false;
      bconchar(result, c);
    }
    else
    {
      bconchar(result, tolower(c));
    }
  }

  return result;
}

extern bstring minsk_syntax_kind_display_name(minsk_syntax_kind_t kind)
{
  switch (kind)
  {
#define X(x)                          \
  case MINSK_SYNTAX_KIND_##x:         \
  {                                   \
    struct tagbstring t;              \
    blk2tbstr(t, #x, sizeof(#x) - 1); \
    return prettify(&t);              \
  }
#include "minsk/code_analysis/syntax/private/kinds.xmacro"
#undef X
  }

  DEBUGGER_FATAL("invalid syntax kind %d", kind);
}
