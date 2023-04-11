#include "minsk/runtime/object.h"

#include <arena.h>
#include <ctype.h>
#include <inttypes.h>
#include <minsk-platform/debugger.h>
#include <minsk-string/string.h>
#include <stdbool.h>

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
minsk_object_type_display_name(Arena* arena, minsk_object_type_t type)
{
  switch (type)
  {
#define X(x)                                \
  case MINSK_OBJECT_TYPE_##x:               \
  {                                         \
    return prettify(arena, STRING_REF(#x)); \
  }
#include "minsk/runtime/private/object_types.xmacro"
#undef X
  }

  DEBUGGER_FATAL("invalid object type %d", type);
}

extern int minsk_object_show(minsk_object_t object, FILE* stream)
{
  switch (object.type)
  {
    case MINSK_OBJECT_TYPE_NIL: return fprintf(stream, "nil");
    case MINSK_OBJECT_TYPE_INTEGER:
      return fprintf(stream, "%" PRId64, object.integer);
  }

  DEBUGGER_FATAL("invalid object type %d", object.type);
}
