#pragma once

#include <arena.h>
#include <minsk/data_structures/buf.h>
#include <stddef.h>
#include <stdint.h>

typedef struct
{
  Arena * arena;
  uint64_t * ptr;
  size_t len;
  size_t cap;
} bitset_t;

extern bitset_t *
bitset_create(Arena * arena);

extern bitset_t *
bitset_create_cap(Arena * arena, size_t cap);

extern bool
bitset_grow(bitset_t * b, size_t newsize);

static inline void
bitset_set(bitset_t * b, size_t i)
{
  size_t shifted_i = i / 64;
  if (shifted_i >= b->len)
  {
    if (!bitset_grow(b, shifted_i + 1))
    {
      return;
    }
  }

  b->ptr[shifted_i] |= UINT64_C(1) << (i % 64);
}

static inline void
bitset_set_value(bitset_t * b, size_t i, bool value)
{
  size_t shifted_i = i / 64;
  uint64_t mask = UINT64_C(1) << (i % 64);
  uint64_t dynmask = (uint64_t)value << (i % 64);
  if (shifted_i >= b->len)
  {
    if (!bitset_grow(b, shifted_i + 1))
    {
      return;
    }
  }
  uint64_t w = b->ptr[shifted_i];
  w &= ~mask;
  w |= dynmask;
  b->ptr[shifted_i] = w;
}

static inline bool
bitset_get(bitset_t const * b, size_t i)
{
  size_t shifted_i = i / 64;
  if (shifted_i >= b->len)
  {
    return false;
  }

  return (b->ptr[shifted_i] & (UINT64_C(1) << (i % 64))) != 0;
}

extern size_t
bitset_count(bitset_t const * b);

static inline size_t
trailing_zeroes(uint64_t w)
{
  if (w == 0)
  {
    return 0;
  }
  size_t t = 1;
  size_t r = 0;
  while ((w & t) == 0)
  {
    t <<= 1;
    r++;
  }
  return r;
}

static inline bool
bitset_next_set_bit(bitset_t const * b, size_t * i)
{
  size_t shifted_i = *i / 64;
  if (shifted_i >= b->len)
  {
    return false;
  }

  uint64_t w = b->ptr[shifted_i];
  w >>= (*i % 64);
  if (w != 0)
  {
    *i += trailing_zeroes(w);
    return true;
  }
  shifted_i++;
  while (shifted_i < b->len)
  {
    w = b->ptr[shifted_i];
    if (w != 0)
    {
      *i = shifted_i * 64 + trailing_zeroes(w);
      return true;
    }
    shifted_i++;
  }
  return false;
}
