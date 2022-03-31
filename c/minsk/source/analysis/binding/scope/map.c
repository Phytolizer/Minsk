#include "minsk/analysis/binding/scope/map.h"
#include "minsk/analysis/symbol.h"
#include "sds.h"
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

static uint64_t bound_scope_map_hash_fnv1a(const sds name) {
  uint64_t h = 0xcbf29ce484222325ULL;
  for (size_t i = 0; i < sdslen(name); i++) {
    h ^= name[i];
    h *= 0x100000001b3ULL;
  }
  return h;
}

static bound_scope_map_bucket_t* bound_scope_map_find_bucket(
    bound_scope_map_bucket_t* buckets, size_t capacity, const sds name) {
  // find matching or empty bucket
  if (capacity == 0) {
    return NULL;
  }
  size_t index = bound_scope_map_hash_fnv1a(name) % capacity;
  while (buckets[index].name != NULL) {
    if (sdscmp(buckets[index].name, name) == 0) {
      return &buckets[index];
    }
    index = (index + 1) % capacity;
  }
  return &buckets[index];
}

void bound_scope_map_init(bound_scope_map_t* map) {
  map->data = NULL;
  map->length = 0;
  map->capacity = 0;
}

void bound_scope_map_insert(
    bound_scope_map_t* map, sds name, variable_symbol_t value) {
  if (map->length >= map->capacity) {
    size_t new_capacity = map->capacity * 2;
    bound_scope_map_bucket_t* new_data =
        malloc(sizeof(bound_scope_map_bucket_t) * new_capacity);
    assert(new_data != NULL);
    // rehash
    for (size_t i = 0; i < new_capacity; i++) {
      new_data[i].name = NULL;
    }
    for (size_t i = 0; i < map->capacity; i++) {
      if (map->data[i].name != NULL) {
        bound_scope_map_bucket_t* bucket = bound_scope_map_find_bucket(
            new_data, new_capacity, map->data[i].name);
        *bucket = map->data[i];
      }
    }
    free(map->data);
    map->data = new_data;
    map->capacity = new_capacity;
  }
  bound_scope_map_bucket_t* bucket =
      bound_scope_map_find_bucket(map->data, map->capacity, name);
  bucket->name = name;
  bucket->value = value;
  map->length++;
}

variable_symbol_t* bound_scope_map_lookup(
    const bound_scope_map_t* map, const sds name) {
  bound_scope_map_bucket_t* bucket =
      bound_scope_map_find_bucket(map->data, map->capacity, name);

  if (bucket == NULL || bucket->name == NULL) {
    return NULL;
  }
  return &bucket->value;
}

void bound_scope_map_free(bound_scope_map_t* map) {
  for (size_t i = 0; i < map->capacity; i++) {
    if (map->data[i].name != NULL) {
      sdsfree(map->data[i].name);
      variable_symbol_free(&map->data[i].value);
    }
  }
  free(map->data);
}
