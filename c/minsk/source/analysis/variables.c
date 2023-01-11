//! A hash table for variables.
//! Key type: variable_symbol_t (string and type pair)
//! Value type: object_t * (nullable during binding)

#include "minsk/analysis/variables.h"
#include "minsk/analysis/symbol.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LOAD_FACTOR 0.75

static uint64_t hash_fnv1a_init(void) {
  return 0xcbf29ce484222325ULL;
}

static void
hash_fnv1a_continue(uint64_t* hash, const void* data, size_t length) {
  uint64_t h = *hash;
  const char* str = data;
  for (size_t i = 0; i < length; ++i) {
    h ^= str[i];
    h *= 0x100000001b3ULL;
  }
  *hash = h;
}

static uint64_t hash_variable_symbol(const variable_symbol_t* symbol) {
  uint64_t h = hash_fnv1a_init();
  hash_fnv1a_continue(&h, symbol->name, sdslen(symbol->name));
  hash_fnv1a_continue(&h, &symbol->type, sizeof(symbol->type));
  return h;
}

static variable_map_bucket_t* variable_map_find_bucket(
    variable_map_bucket_t* buckets,
    size_t bucket_count,
    variable_symbol_t variable
) {
  // Finds a bucket for insertion or lookup using linear probing.
  uint64_t h = hash_variable_symbol(&variable);
  size_t i = h % bucket_count;
  while (buckets[i].variable.name != NULL) {
    if (buckets[i].variable.type == variable.type &&
        strcmp(buckets[i].variable.name, variable.name) == 0) {
      return &buckets[i];
    }
    i = (i + 1) % bucket_count;
  }
  return &buckets[i];
}

void variable_map_init(variable_map_t* map) {
  map->buckets = NULL;
  map->length = 0;
  map->capacity = 0;
}

void variable_map_free(variable_map_t* map) {
  for (size_t i = 0; i < map->capacity; ++i) {
    variable_map_bucket_t* bucket = &map->buckets[i];
    if (bucket->variable.name != NULL) {
      variable_symbol_free(&bucket->variable);
    }
    if (bucket->value != NULL) {
      object_free(bucket->value);
    }
  }
  free(map->buckets);
}

void variable_map_insert(
    variable_map_t* map,
    variable_symbol_t variable,
    object_t* value
) {
  if ((double)map->length + 1 > (double)map->capacity * MAX_LOAD_FACTOR) {
    size_t new_capacity = map->capacity * 2;
    if (new_capacity == 0) {
      new_capacity = 1;
    }
    variable_map_bucket_t* new_buckets =
        malloc(new_capacity * sizeof(variable_map_bucket_t));
    for (size_t i = 0; i < new_capacity; ++i) {
      new_buckets[i].variable.name = NULL;
      new_buckets[i].value = NULL;
    }
    for (size_t i = 0; i < map->capacity; ++i) {
      variable_map_bucket_t* bucket = &map->buckets[i];
      if (bucket->variable.name != NULL) {
        variable_map_bucket_t* new_bucket = variable_map_find_bucket(
            new_buckets,
            new_capacity,
            bucket->variable
        );
        new_bucket->variable = bucket->variable;
        new_bucket->value = bucket->value;
      }
    }
    free(map->buckets);
    map->buckets = new_buckets;
    map->capacity = new_capacity;
  }

  variable_map_bucket_t* bucket =
      variable_map_find_bucket(map->buckets, map->capacity, variable);
  if (bucket->variable.name != NULL) {
    variable_symbol_free(&bucket->variable);
    --map->length;
  }
  if (bucket->value != NULL) {
    object_free(bucket->value);
  }
  bucket->variable = variable;
  bucket->value = value;
  ++map->length;
}

variable_map_bucket_t*
variable_map_find(variable_map_t* map, variable_symbol_t variable) {
  if (map->capacity == 0) {
    return NULL;
  }
  variable_map_bucket_t* bucket =
      variable_map_find_bucket(map->buckets, map->capacity, variable);
  if (bucket->variable.name == NULL) {
    return NULL;
  }
  return bucket;
}

variable_map_bucket_t*
variable_map_find_by_name(variable_map_t* map, const char* name) {
  for (size_t i = 0; i < map->capacity; i++) {
    variable_map_bucket_t* bucket = &map->buckets[i];
    if (bucket->variable.name != NULL &&
        strcmp(bucket->variable.name, name) == 0) {
      return bucket;
    }
  }
  return NULL;
}
