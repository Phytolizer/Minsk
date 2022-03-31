#pragma once

#include "minsk/analysis/symbol.h"
#include "sds.h"

typedef struct {
  sds name;
  variable_symbol_t value;
} bound_scope_map_bucket_t;

typedef struct {
  bound_scope_map_bucket_t* data;
  size_t length;
  size_t capacity;
} bound_scope_map_t;

void bound_scope_map_init(bound_scope_map_t* map);
void bound_scope_map_insert(
    bound_scope_map_t* map, sds name, variable_symbol_t value);
variable_symbol_t* bound_scope_map_lookup(
    const bound_scope_map_t* map, const sds name);
void bound_scope_map_free(bound_scope_map_t* map);
