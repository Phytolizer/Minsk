#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#define BUF_T(T, name) \
  struct name##_buf    \
  {                    \
    T* ptr;            \
    size_t len;        \
    size_t cap;        \
    bool is_ref;       \
  }

#define BUF_REF(T, p, size)                                  \
  (T)                                                        \
  {                                                          \
    .ptr = (p), .len = (size), .cap = (size), .is_ref = true \
  }

#define BUF_ARRAY(T, arr)                                             \
  (T)                                                                 \
  {                                                                   \
    .ptr = (arr), .len = sizeof(arr) / sizeof(*(arr)), .is_ref = true \
  }

#define BUF_OWNER(T, p, size)                                 \
  (T)                                                         \
  {                                                           \
    .ptr = (p), .len = (size), .cap = (size), .is_ref = false \
  }

#define BUF_PUSH(buf, val)                                                \
  do                                                                      \
  {                                                                       \
    if ((buf)->len == (buf)->cap)                                         \
    {                                                                     \
      (buf)->cap = (buf)->cap ? (buf)->cap * 2 : 1;                       \
      (buf)->ptr = realloc((buf)->ptr, (buf)->cap * sizeof(*(buf)->ptr)); \
    }                                                                     \
    (buf)->ptr[(buf)->len++] = (val);                                     \
  } while (false)

#define BUF_RESERVE(buf, size)                                            \
  do                                                                      \
  {                                                                       \
    if ((buf)->cap < (size))                                              \
    {                                                                     \
      (buf)->cap = (size);                                                \
      (buf)->ptr = realloc((buf)->ptr, (buf)->cap * sizeof(*(buf)->ptr)); \
    }                                                                     \
  } while (false)

#define BUF_APPEND(buf, other)                   \
  do                                             \
  {                                              \
    __auto_type other_ = (other);                \
    BUF_RESERVE(buf, (buf)->len + (other_).len); \
    memcpy(                                      \
      (buf)->ptr + (buf)->len,                   \
      (other_).ptr,                              \
      (other_).len * sizeof(*(buf)->ptr)         \
    );                                           \
    (buf)->len += (other_).len;                  \
  } while (false)

#define BUF_FREE(buf)  \
  do                   \
  {                    \
    if (!(buf).is_ref) \
    {                  \
      free((buf).ptr); \
    }                  \
  } while (false)

#ifdef __GNUC__
#define BUF_LIT(T, ...)                                                  \
  ({                                                                     \
    typeof(*((T){0}.ptr)) arr_[] = {__VA_ARGS__};                        \
    size_t arrlen_ = sizeof(arr_) / sizeof(*arr_);                       \
    void* newbuf = malloc(sizeof(arr_));                                 \
    memcpy(newbuf, arr_, sizeof(arr_));                                  \
    (T){.ptr = newbuf, .len = arrlen_, .cap = arrlen_, .is_ref = false}; \
  })
#endif  // __GNUC__
