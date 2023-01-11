//! A hash set for the simple syntax_kind_t type.
//! Uses FNV-1A internally.
#include "minsk_test/analysis/syntax/kind_set.h"
#include "minsk/analysis/syntax/kind.h"
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#define MAX_LOAD_FACTOR 0.75

static uint64_t syntax_kind_hash_fnv1a(const void* raw_key) {
  uint64_t h = 0xcbf29ce484222325ULL;
  const char* key = raw_key;
  for (size_t i = 0; i < sizeof(syntax_kind_t); i++) {
    h ^= key[i];
    h *= 0x100000001b3ULL;
  }
  return h;
}

static syntax_kind_set_bucket_t* find_bucket(
    syntax_kind_set_bucket_t* buckets,
    size_t capacity,
    syntax_kind_t kind
) {
  // find matching or empty bucket
  size_t index = syntax_kind_hash_fnv1a(&kind) % capacity;
  while (buckets[index].present) {
    if (buckets[index].kind == kind) {
      return &buckets[index];
    }
    index = (index + 1) % capacity;
  }
  return &buckets[index];
}

void syntax_kind_set_init(syntax_kind_set_t* set) {
  set->data = malloc(sizeof(syntax_kind_set_bucket_t) * 8);
  set->length = 0;
  set->capacity = 8;
  for (size_t i = 0; i < set->capacity; i++) {
    set->data[i].present = false;
  }
}

void syntax_kind_set_insert(syntax_kind_set_t* set, syntax_kind_t kind) {
  if ((double)set->length >= (double)set->capacity * MAX_LOAD_FACTOR) {
    size_t new_capacity = set->capacity * 2;
    syntax_kind_set_bucket_t* new_data =
        malloc(sizeof(syntax_kind_set_bucket_t) * new_capacity);
    assert(new_data != NULL);
    // rehash
    for (size_t i = 0; i < new_capacity; i++) {
      new_data[i].present = false;
    }
    for (size_t i = 0; i < set->capacity; i++) {
      if (set->data[i].present) {
        syntax_kind_set_bucket_t* bucket =
            find_bucket(new_data, new_capacity, set->data[i].kind);
        *bucket = set->data[i];
      }
    }
    free(set->data);
    set->data = new_data;
    set->capacity = new_capacity;
  }

  syntax_kind_set_bucket_t* bucket =
      find_bucket(set->data, set->capacity, kind);
  if (!bucket->present) {
    bucket->present = true;
    bucket->kind = kind;
    set->length++;
  }
}

bool syntax_kind_set_contains(
    const syntax_kind_set_t* set,
    syntax_kind_t kind
) {
  uint64_t hash = syntax_kind_hash_fnv1a(&kind);
  // open addressing with linear probing
  for (size_t i = 0; i < set->capacity; i++) {
    size_t index = (hash + i) % set->capacity;
    if (!set->data[index].present) {
      break;
    }
    if (set->data[index].kind == kind) {
      return true;
    }
  }
  return false;
}

void syntax_kind_set_free(syntax_kind_set_t* set) {
  free(set->data);
}
