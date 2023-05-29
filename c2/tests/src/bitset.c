#include "minsk-test/bitset.h"

#include <string.h>

extern bitset_t *
bitset_create(Arena * arena)
{
  bitset_t * b = arena_alloc(arena, sizeof(bitset_t));
  if (b == NULL)
  {
    return NULL;
  }

  b->arena = arena;
  b->ptr = NULL;
  b->len = 0;
  b->cap = 0;
  return b;
}

extern bitset_t *
bitset_create_cap(Arena * arena, size_t cap)
{
  bitset_t * b = arena_alloc(arena, sizeof(bitset_t));
  if (b == NULL)
  {
    return NULL;
  }

  b->arena = arena;
  b->len = (cap + sizeof(uint64_t) * 8 - 1) / (sizeof(uint64_t) * 8);
  b->cap = b->len;
  b->ptr = arena_alloc(arena, b->len * sizeof(uint64_t));
  if (b->ptr == NULL)
  {
    return NULL;
  }
  memset(b->ptr, 0, b->len * sizeof(uint64_t));

  return b;
}

extern bool
bitset_grow(bitset_t * b, size_t newsize)
{
  if (newsize < b->len)
  {
    return false;
  }
  if (newsize > SIZE_MAX / 64)
  {
    return false;
  }
  if (b->cap < newsize)
  {
    size_t newcap = b->cap;
    if (newcap == 0)
    {
      newcap = 1;
    }
    while (newcap < newsize)
    {
      newcap *= 2;
    }
    uint64_t * newptr = arena_realloc(
      b->arena,
      b->ptr,
      b->cap * sizeof(uint64_t),
      newcap * sizeof(uint64_t)
    );
    if (newptr == NULL)
    {
      return false;
    }
    b->cap = newcap;
    b->ptr = newptr;
  }
  memset(b->ptr + b->len, 0, (newsize - b->len) * sizeof(uint64_t));
  b->len = newsize;
  return true;
}

static size_t
count_set_bits(uint64_t x)
{
  // only count bits that are 1
  size_t count = 0;
  while (x != 0)
  {
    count += x & 1;
    x >>= 1;
  }
  return count;
}

extern size_t
bitset_count(bitset_t const * b)
{
  size_t count = 0;
  for (size_t i = 0; i < b->len; ++i)
  {
    if (b->ptr[i] != 0)
    {
      count += count_set_bits(b->ptr[i]);
    }
  }
  return count;
}
