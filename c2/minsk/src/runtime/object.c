#include "minsk/runtime/object.h"

#include <arena.h>
#include <inttypes.h>
#include <minsk-platform/debugger.h>
#include <minsk-string/string.h>

#include "minsk/meta/snake_to_camel.h"

extern string_t
minsk_object_type_display_name(Arena * arena, minsk_object_type_t type)
{
  switch (type)
  {
#define X(x)                                      \
  case MINSK_OBJECT_TYPE_##x:                     \
  {                                               \
    return snake_to_camel(arena, STRING_REF(#x)); \
  }
#include "minsk/runtime/private/object_types.xmacro"
#undef X
  }

  DEBUGGER_FATAL("invalid object type %d", type);
}

extern int minsk_object_show(minsk_object_t object, FILE * stream)
{
  switch (object.type)
  {
    case MINSK_OBJECT_TYPE_NIL: return fprintf(stream, "nil");
    case MINSK_OBJECT_TYPE_INTEGER:
      return fprintf(stream, "%" PRId64, object.integer);
    case MINSK_OBJECT_TYPE_BOOLEAN:
      return fprintf(
        stream,
        STRING_FMT,
        STRING_ARG(object.boolean ? STRING_REF("true") : STRING_REF("false"))
      );
  }

  DEBUGGER_FATAL("invalid object type %d", object.type);
}
