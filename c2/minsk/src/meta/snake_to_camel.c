#include "minsk/meta/snake_to_camel.h"

#include <arena.h>
#include <ctype.h>
#include <minsk-string/string.h>
#include <stdbool.h>
#include <stddef.h>

extern string_t
snake_to_camel(Arena * arena, string_t str)
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
