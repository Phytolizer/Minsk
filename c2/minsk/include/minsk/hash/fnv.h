#pragma once

#include <stddef.h>
#include <stdint.h>

static inline uint64_t
minsk_hash_fnv64a(void const * data, size_t size)
{
  uint64_t hash = 0xCBF29CE484222325;
  unsigned char const * ptr = (unsigned char const *)data;

  for (size_t i = 0; i < size; ++i)
  {
    hash ^= ptr[i];
    hash *= 0x100000001B3;
  }

  return hash;
}
