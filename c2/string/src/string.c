#include "minsk-string/string.h"

#include <arena.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void* string_alloc(Arena* maybe_arena, size_t size)
{
  if (maybe_arena != NULL)
  {
    return arena_alloc(maybe_arena, size);
  }
  return malloc(size);
}

static void* string_realloc(Arena* maybe_arena, void* ptr, size_t size)
{
  if (maybe_arena != NULL)
  {
    return arena_realloc(maybe_arena, ptr, 0, size);
  }
  return realloc(ptr, size);
}

static string_t
string_vprintf_arena(Arena* arena, const char* fmt, va_list args)
{
  va_list args2;
  va_copy(args2, args);
  int length = vsnprintf(0, 0, fmt, args2);
  va_end(args2);

  string_t str = {
    .data = string_alloc(arena, length + 1),
    .length = length,
    .capacity = length + 1,
  };

  vsnprintf(str.data, str.capacity, fmt, args);

  str.length = length;

  return str;
}

extern string_t string_printf_arena(Arena* arena, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  string_t result = string_vprintf_arena(arena, fmt, args);
  va_end(args);

  return result;
}

extern string_t string_printf(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  string_t result = string_vprintf_arena(NULL, fmt, args);
  va_end(args);

  return result;
}

extern string_t string_dup_arena(Arena* arena, string_t s)
{
  char* result = string_alloc(arena, s.length);
  memcpy(result, s.data, s.length);
  return STRING_OWN_DATA(result, s.length, s.length);
}

extern string_t string_dup(string_t s)
{
  return string_dup_arena(NULL, s);
}

extern void string_append_arena(Arena* arena, string_t* buf, string_t arg)
{
  if (arg.length == 0) return;

  size_t old_cap = buf->capacity;
  while (buf->length + arg.length > buf->capacity)
  {
    buf->capacity = buf->capacity * 2 + 1;
  }
  if (buf->capacity > old_cap)
  {
    buf->data = string_realloc(arena, buf->data, buf->capacity);
  }
  memcpy(buf->data + buf->length, arg.data, arg.length);
  buf->length += arg.length;
}

extern void string_append(string_t* buf, string_t arg)
{
  string_append_arena(NULL, buf, arg);
}

static void string_append_vprintf_arena(
  Arena* arena,
  string_t* buf,
  const char* fmt,
  va_list args
)
{
  va_list args2;
  va_copy(args2, args);
  int length = vsnprintf(0, 0, fmt, args2);
  va_end(args2);

  size_t old_cap = buf->capacity;
  while (buf->length + length > buf->capacity)
  {
    buf->capacity = buf->capacity * 2 + 1;
  }
  if (buf->capacity > old_cap)
  {
    buf->data = string_realloc(arena, buf->data, buf->capacity);
  }

  vsnprintf(buf->data + buf->length, buf->capacity - buf->length, fmt, args);

  buf->length += length;
}

extern void
string_append_printf_arena(Arena* arena, string_t* buf, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  string_append_vprintf_arena(arena, buf, fmt, args);
  va_end(args);
}

extern void string_append_printf(string_t* buf, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  string_append_vprintf_arena(NULL, buf, fmt, args);
  va_end(args);
}

extern string_t string_term_arena(Arena* arena, string_t s, char termchar)
{
  if (s.capacity > s.length && s.data[s.length] == termchar)
  {
    // avoid copy
    return STRING_AS_REF(s);
  }
  char* new_data = string_alloc(arena, s.length + 1);
  memcpy(new_data, s.data, s.length);
  new_data[s.length] = termchar;
  return STRING_OWN_DATA(new_data, s.length, s.length + 1);
}

extern string_t string_term(string_t s, char termchar)
{
  return string_term_arena(NULL, s, termchar);
}

extern void string_free_arena(Arena* arena, string_t s)
{
  if (arena == NULL && !s.is_ref)
  {
    free(s.data);
  }
}

extern void string_free(string_t s)
{
  string_free_arena(NULL, s);
}

extern void string_reserve_arena(Arena* arena, string_t* s, size_t len)
{
  if (s->capacity < len)
  {
    s->data = string_realloc(arena, s->data, len);
    s->capacity = len;
  }
}

extern void string_reserve(string_t* s, size_t len)
{
  string_reserve_arena(NULL, s, len);
}
