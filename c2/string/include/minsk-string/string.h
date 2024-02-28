#pragma once

#include <arena.h>
#include <stdbool.h>
#include <stddef.h>  // IWYU pragma: keep
#include <stdio.h>  // IWYU pragma: keep
#include <stdlib.h>  // IWYU pragma: keep
#include <string.h>  // IWYU pragma: keep

typedef struct
{
  char * data;
  size_t length;
  size_t capacity;
  bool is_ref;
} string_t;

#define EMPTY_STRING ((string_t){NULL, 0, 0, false})

#define STRING_REF_C(str) \
  { \
    (char *)(str), sizeof(str) - 1, sizeof(str) - 1, true \
  }

#define STRING_REF(str) ((string_t)STRING_REF_C(str))

#define STRING_REF_FROM_C(str) \
  ({ \
    size_t _len = strlen(str); \
    ((string_t){(char *)(str), _len, _len, true}); \
  })

#define STRING_REF_DATA(str, length) \
  (string_t) \
  { \
    (char *)(str), (length), (length), true \
  }

#define STRING_AS_REF(str) STRING_REF_DATA((str).data, (str).length)

#define STRING_SUB(str, a, b) STRING_REF_DATA((str).data + (a), (b) - (a))
#define STRING_SUB_LEN(str, a, len) STRING_REF_DATA((str).data + (a), (len))
#define STRING_SUB_AFTER(str, a) STRING_SUB((str), (a), (str).length)

#define STRING_OWN_DATA(str, length, alloc_length) \
  (string_t) \
  { \
    (str), (length), (alloc_length), false \
  }

#define STRING_EQUAL(a, b) \
  ((a).length == (b).length && memcmp((a).data, (b).data, (a).length) == 0)

static inline bool
string_endswith(string_t a, string_t b)
{
  if (a.length < b.length)
  {
    return false;
  }

  return memcmp(a.data + a.length - b.length, b.data, b.length) == 0;
}

extern string_t
string_printf_arena(Arena * arena, const char * fmt, ...);
extern string_t
string_printf(const char * fmt, ...);
extern string_t
string_dup_arena(Arena * arena, string_t s);
extern string_t
string_dup(string_t s);
extern void
string_append_arena(Arena * arena, string_t * buf, string_t arg);
extern void
string_append(string_t * buf, string_t arg);
extern void
string_append_printf_arena(
  Arena * arena,
  string_t * buf,
  const char * fmt,
  ...
);
extern void
string_append_printf(string_t * buf, const char * fmt, ...);
extern void
string_push_arena(Arena * arena, string_t * buf, char c);
extern void
string_push(string_t * buf, char c);

// Return a ref string that is definitely terminated. May/may not require
// allocation.
extern string_t
string_term_arena(Arena * arena, string_t s, char termchar);
extern string_t
string_term(string_t s, char termchar);

extern void
string_free_arena(Arena * arena, string_t s);
extern void
string_free(string_t s);

extern void
string_reserve_arena(Arena * arena, string_t * s, size_t len);
extern void
string_reserve(string_t * s, size_t len);

#define STRING_FMT "%.*s"
#define STRING_ARG(str) (int)(str).length, (str).data
