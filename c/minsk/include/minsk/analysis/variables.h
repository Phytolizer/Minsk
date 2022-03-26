#pragma once

#include "minsk/analysis/symbol.h"
#include "minsk/runtime/object.h"

typedef struct {
  variable_symbol_t variable;
  object_t *value;
} variable_map_bucket_t;

typedef struct {
  variable_map_bucket_t *buckets;
  size_t length;
  size_t capacity;
} variable_map_t;

void variable_map_init(variable_map_t *map);
void variable_map_free(variable_map_t *map);
void variable_map_insert(variable_map_t *map, variable_symbol_t variable,
                         object_t *value);
variable_map_bucket_t *variable_map_find(variable_map_t *map,
                                         variable_symbol_t variable);
variable_map_bucket_t *variable_map_find_by_name(variable_map_t *map,
                                                 const char *name);
