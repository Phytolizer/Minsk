#pragma once

#include <arena.h>
#include <minsk-string/string.h>
#include <uthash.h>

#include "minsk/runtime/object.h"

typedef struct
{
  string_t key;
  minsk_object_t value;
  UT_hash_handle hh;
} minsk_variable_bucket_t;

typedef struct
{
  Arena * _arena;
  minsk_variable_bucket_t * _buckets;
} minsk_variable_map_t;

extern minsk_variable_map_t
minsk_variable_map_new(Arena * arena);

extern void
minsk_variable_map_put(
  minsk_variable_map_t * map,
  string_t key,
  minsk_object_t value
);

bool
minsk_variable_map_try_get_value(
  minsk_variable_map_t * map,
  string_t key,
  minsk_object_t * out_value
);
