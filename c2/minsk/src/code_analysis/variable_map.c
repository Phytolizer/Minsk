#include "minsk/code_analysis/variable_map.h"

#include <arena.h>
#include <minsk-string/string.h>
#include <stdint.h>

#include "minsk/hash/fnv.h"

static uint64_t
variable_symbol_hash(minsk_variable_symbol_t v);
static bool
variable_symbol_cmpr(minsk_variable_symbol_t a, minsk_variable_symbol_t b);

#define NAME minsk_variable_map
#define KEY_TY minsk_variable_symbol_t
#define VAL_TY minsk_object_t
#define HASH_FN variable_symbol_hash
#define CMPR_FN variable_symbol_cmpr
#define CTX_TY Arena *
#define MAX_LOAD 0.8
#define IMPLEMENTATION_MODE
#include <verstable.h>

static uint64_t
variable_symbol_hash(minsk_variable_symbol_t v)
{
  // The hash of a symbol is the hash of its name. Type is not considered
  // except when comparing symbols for equality.
  return minsk_hash_fnv64a(v.name.data, v.name.length);
}

static bool
variable_symbol_cmpr(minsk_variable_symbol_t a, minsk_variable_symbol_t b)
{
  return a.type == b.type && STRING_EQUAL(a.name, b.name);
}

extern minsk_variable_map_t
minsk_variable_map_new(Arena * arena)
{
  minsk_variable_map_t map;
  minsk_variable_map_init(&map, arena);
  return map;
}

extern void
minsk_variable_map_put(
  minsk_variable_map_t * map,
  minsk_variable_symbol_t key,
  minsk_object_t value
)
{
  minsk_variable_symbol_t key_dup = {
    .name = string_dup_arena(map->ctx, key.name),
    .type = key.type,
  };
  minsk_variable_map_insert(map, key_dup, value);
}

bool
minsk_variable_map_try_get_value(
  minsk_variable_map_t * map,
  minsk_variable_symbol_t key,
  minsk_object_t * out_value
)
{
  minsk_variable_map_itr itr = minsk_variable_map_get(map, key);
  if (minsk_variable_map_is_end(itr))
  {
    return false;
  }

  *out_value = itr.data->val;
  return true;
}

bool
minsk_variable_map_find_by_name(
  minsk_variable_map_t * map,
  string_t name,
  minsk_variable_symbol_t * out_key
)
{
  for (minsk_variable_map_itr itr = minsk_variable_map_first(map);
       !minsk_variable_map_is_end(itr);
       itr = minsk_variable_map_next(itr))
  {
    if (STRING_EQUAL(itr.data->key.name, name))
    {
      *out_key = itr.data->key;
      return true;
    }
  }
  return false;
}

void
minsk_variable_map_del(minsk_variable_map_t * map, minsk_variable_symbol_t key)
{
  minsk_variable_map_erase(map, key);
}
