#include "minsk/code_analysis/variable_map.h"

#include <arena.h>
#include <minsk-string/string.h>
#include <stddef.h>
#include <uthash.h>

#undef uthash_malloc
#define uthash_malloc(sz) arena_alloc(map->_arena, (sz))

#undef uthash_free
#define uthash_free(ptr, sz)

static unsigned
variable_symbol_hash(minsk_variable_symbol_t * v)
{
  unsigned hashv;
  HASH_FUNCTION(v->name.data, v->name.length, hashv);
  return hashv;
}

static bool
variable_symbol_equal(minsk_variable_symbol_t * a, minsk_variable_symbol_t * b)
{
  return a->type == b->type && STRING_EQUAL(a->name, b->name);
}

#undef HASH_FUNCTION
#define HASH_FUNCTION(s, len, hashv) \
  ((hashv) = variable_symbol_hash((minsk_variable_symbol_t *)s))

#undef HASH_KEYCMP
#define HASH_KEYCMP(a, b, len)    \
  (!variable_symbol_equal(        \
    (minsk_variable_symbol_t *)a, \
    (minsk_variable_symbol_t *)b  \
  ))

extern minsk_variable_map_t
minsk_variable_map_new(Arena * arena)
{
  return (minsk_variable_map_t){._arena = arena};
}

extern void
minsk_variable_map_put(
  minsk_variable_map_t * map,
  minsk_variable_symbol_t key,
  minsk_object_t value
)
{
  minsk_variable_bucket_t * bucket = arena_alloc(map->_arena, sizeof(*bucket));
  bucket->key = (minsk_variable_symbol_t){
    string_dup_arena(map->_arena, key.name),
    key.type,
  };
  bucket->value = value;
  HASH_ADD(hh, map->_buckets, key, sizeof(key), bucket);
}

bool
minsk_variable_map_try_get_value(
  minsk_variable_map_t * map,
  minsk_variable_symbol_t key,
  minsk_object_t * out_value
)
{
  minsk_variable_bucket_t * bucket = NULL;
  HASH_FIND(hh, map->_buckets, &key, sizeof(key), bucket);
  if (bucket == NULL)
  {
    return false;
  }

  *out_value = bucket->value;
  return true;
}

bool
minsk_variable_map_find_by_name(
  minsk_variable_map_t * map,
  string_t name,
  minsk_variable_symbol_t * out_key
)
{
  minsk_variable_bucket_t * el;
  minsk_variable_bucket_t * tmp;
  HASH_ITER(hh, map->_buckets, el, tmp)
  {
    if (STRING_EQUAL(el->key.name, name))
    {
      *out_key = el->key;
      return true;
    }
  }
  return false;
}

void
minsk_variable_map_del(minsk_variable_map_t * map, minsk_variable_symbol_t key)
{
  minsk_variable_bucket_t * bucket = NULL;
  HASH_FIND(hh, map->_buckets, &key, sizeof(key), bucket);
  if (bucket != NULL)
  {
    HASH_DEL(map->_buckets, bucket);
  }
}
