#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#include "minsk-platform/debugger.h"

#define BUF_T(T) \
  struct         \
  {              \
    T * ptr;     \
    size_t len;  \
    size_t cap;  \
    bool is_ref; \
  }

#define BUF_INIT \
  {              \
    0            \
  }

#define BUF_REF(T, p, size)                                  \
  (T)                                                        \
  {                                                          \
    .ptr = (p), .len = (size), .cap = (size), .is_ref = true \
  }

#define BUF_ARRAY(T, arr)                                                     \
  (T)                                                                         \
  {                                                                           \
    .ptr = (void *)(arr), .len = sizeof(arr) / sizeof(*(arr)), .is_ref = true \
  }

#define BUF_OWNER(T, p, size)                                 \
  (T)                                                         \
  {                                                           \
    .ptr = (p), .len = (size), .cap = (size), .is_ref = false \
  }

#define BUF_PUSH_ARENA(arena, buf, val)                          \
  do                                                             \
  {                                                              \
    if ((buf)->len == (buf)->cap)                                \
    {                                                            \
      (buf)->cap = (buf)->cap ? (buf)->cap * 2 : 1;              \
      if (arena)                                                 \
      {                                                          \
        void * buf__tmp = arena_realloc(                         \
          arena,                                                 \
          (buf)->ptr,                                            \
          (buf)->len * sizeof(*(buf)->ptr),                      \
          (buf)->cap * sizeof(*(buf)->ptr)                       \
        );                                                       \
        DEBUGGER_ASSERT(buf__tmp != NULL);                       \
        (buf)->ptr = buf__tmp;                                   \
      }                                                          \
      else                                                       \
      {                                                          \
        void * buf__tmp =                                        \
          realloc((buf)->ptr, (buf)->cap * sizeof(*(buf)->ptr)); \
        DEBUGGER_ASSERT(buf__tmp != NULL);                       \
        (buf)->ptr = buf__tmp;                                   \
      }                                                          \
    }                                                            \
    (buf)->ptr[(buf)->len++] = (val);                            \
  } while (false)

#define BUF_PUSH(buf, val) BUF_PUSH_ARENA(NULL, buf, val)

#define BUF_POP(buf) ((buf)->ptr[--(buf)->len])

#define BUF_RESERVE_ARENA(arena, buf, size)                                 \
  do                                                                        \
  {                                                                         \
    if ((buf)->cap < (size))                                                \
    {                                                                       \
      (buf)->cap = (size);                                                  \
      if (arena != NULL)                                                    \
      {                                                                     \
        (buf)->ptr = arena_realloc(                                         \
          arena,                                                            \
          (buf)->ptr,                                                       \
          (buf)->len * sizeof(*(buf)->ptr),                                 \
          (buf)->cap * sizeof(*(buf)->ptr)                                  \
        );                                                                  \
      }                                                                     \
      else                                                                  \
      {                                                                     \
        (buf)->ptr = realloc((buf)->ptr, (buf)->cap * sizeof(*(buf)->ptr)); \
      }                                                                     \
    }                                                                       \
  } while (false)

#define BUF_RESERVE(buf, val) BUF_RESERVE_ARENA(NULL, buf, val)

#define BUF_APPEND_ARENA(arena, buf, other)                   \
  do                                                          \
  {                                                           \
    __auto_type other_ = (other);                             \
    if (other_.len == 0)                                      \
    {                                                         \
      break;                                                  \
    }                                                         \
    BUF_RESERVE_ARENA(arena, buf, (buf)->len + (other_).len); \
    memcpy(                                                   \
      (buf)->ptr + (buf)->len,                                \
      (other_).ptr,                                           \
      (other_).len * sizeof(*(buf)->ptr)                      \
    );                                                        \
    (buf)->len += (other_).len;                               \
  } while (false)

#define BUF_APPEND(buf, val) BUF_APPEND_ARENA(NULL, buf, val)

#define BUF_FREE_ARENA(arena, buf) \
  do                               \
  {                                \
    if (!arena && !(buf).is_ref)   \
    {                              \
      free((buf).ptr);             \
    }                              \
  } while (false)

#define BUF_FREE(buf, val) BUF_FREE_ARENA(NULL, buf, val)

#if defined(__GNUC__) || defined(__clang__)
 #define BUF_LIT_C(T, ...)                             \
   ({                                                  \
    typeof(*((T)BUF_INIT.ptr)) arr_[] = {__VA_ARGS__}; \
    BUF_ARRAY(T, arr_);                                \
   })
 #define BUF_LIT_ARENA(arena, T, ...)                                    \
   ({                                                                    \
    typeof(*((T){0}.ptr)) arr_[] = {__VA_ARGS__};                        \
    size_t arrlen_ = sizeof(arr_) / sizeof(*arr_);                       \
    void * newbuf =                                                      \
      arena ? arena_alloc(arena, sizeof(arr_)) : malloc(sizeof(arr_));   \
    DEBUGGER_ASSERT(newbuf != NULL);                                     \
    memcpy(newbuf, arr_, sizeof(arr_));                                  \
    (T){.ptr = newbuf, .len = arrlen_, .cap = arrlen_, .is_ref = false}; \
   })

 #define BUF_LIT(T, ...) BUF_LIT_ARENA(NULL, T, __VA_ARGS__)
#else
 #define BUF_LIT_ARENA(...) error_non_gnu_compiler
 #define BUF_LIT(...) error_non_gnu_compiler
#endif  // __GNUC__
