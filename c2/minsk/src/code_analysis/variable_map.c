#include "minsk/code_analysis/variable_map.h"

#undef uthash_malloc
#define uthash_malloc(sz) arena_alloc(map->_arena, (sz))

#undef uthash_free
#define uthash_free(ptr, sz)

extern minsk_variable_map_t
minsk_variable_map_new(Arena * arena)
{
  return (minsk_variable_map_t){._arena = arena};
}

extern void
minsk_variable_map_put(
  minsk_variable_map_t * map,
  string_t key,
  minsk_object_t value
)
{
  minsk_variable_bucket_t * bucket = arena_alloc(map->_arena, sizeof(*bucket));
  bucket->key = string_dup_arena(map->_arena, key);
  bucket->value = value;
  HASH_ADD_KEYPTR(
    hh,
    map->_buckets,
    bucket->key.data,
    bucket->key.length,
    bucket
  );
}

bool
minsk_variable_map_try_get_value(
  minsk_variable_map_t * map,
  string_t key,
  minsk_object_t * out_value
)
{
  minsk_variable_bucket_t * bucket = NULL;
  HASH_FIND(hh, map->_buckets, key.data, key.length, bucket);
  if (bucket == NULL)
  {
    return false;
  }

  *out_value = bucket->value;
  return true;
}
