#pragma once

#include <arena.h>
#include <stdbool.h>
#include <stddef.h>  // IWYU pragma: keep
#include <stdio.h>   // IWYU pragma: keep
#include <stdlib.h>  // IWYU pragma: keep
#include <string.h>  // IWYU pragma: keep

typedef struct
{
  char* data;
  size_t length;
  size_t capacity;
  bool is_ref;
} string_t;

#define EMPTY_STRING  \
  {                   \
    NULL, 0, 0, false \
  }

#define STRING_REF_C(str)                                \
  {                                                      \
    (char*)(str), sizeof(str) - 1, sizeof(str) - 1, true \
  }

#define STRING_REF(str) ((string_t)STRING_REF_C(str))

#define STRING_REF_FROM_C(str)                    \
  ({                                              \
    size_t _len = strlen(str);                    \
    ((string_t){(char*)(str), _len, _len, true}); \
  })

#define STRING_REF_DATA(str, length)       \
  (string_t)                               \
  {                                        \
    (char*)(str), (length), (length), true \
  }

#define STRING_AS_REF(str) STRING_REF_DATA((str).data, (str).length)

#define STRING_OWN_DATA(str, length, alloc_length) \
  (string_t)                                       \
  {                                                \
    (str), (length), (alloc_length), false         \
  }

#define STRING_EQUAL(a, b) \
  ((a).length == (b).length && memcmp((a).data, (b).data, (a).length) == 0)

extern string_t string_printf_arena(Arena* arena, const char* fmt, ...);
extern string_t string_printf(const char* fmt, ...);
extern string_t string_dup_arena(Arena* arena, string_t s);
extern string_t string_dup(string_t s);
extern void string_append_arena(Arena* arena, string_t* buf, string_t arg);
extern void string_append(string_t* buf, string_t arg);
extern void
string_append_printf_arena(Arena* arena, string_t* buf, const char* fmt, ...);
extern void string_append_printf(string_t* buf, const char* fmt, ...);

// Return a ref string that is definitely terminated. May/may not require
// allocation.
extern string_t string_term_arena(Arena* arena, string_t s, char termchar);
extern string_t string_term(string_t s, char termchar);

extern void string_free_arena(Arena* arena, string_t s);
extern void string_free(string_t s);

extern void string_reserve_arena(Arena* arena, string_t* s, size_t len);
extern void string_reserve(string_t* s, size_t len);

#define STRING_FMT "%.*s"
#define STRING_ARG(str) (int)(str).length, (str).data
