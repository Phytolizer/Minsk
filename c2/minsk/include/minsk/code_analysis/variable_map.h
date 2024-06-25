#pragma once

#include <arena.h>
#include <minsk-string/string.h>
#include <stdbool.h>

#include "minsk/code_analysis/variable_symbol.h"
#include "minsk/runtime/object.h"

#define NAME minsk_variable_map
#define KEY_TY minsk_variable_symbol_t
#define VAL_TY minsk_object_t
#define CTX_TY Arena *
#define HEADER_MODE
#include <verstable.h>

typedef minsk_variable_map minsk_variable_map_t;

extern minsk_variable_map_t
minsk_variable_map_new(Arena * arena);

extern void
minsk_variable_map_put(
  minsk_variable_map_t * map,
  minsk_variable_symbol_t key,
  minsk_object_t value
);

bool
minsk_variable_map_try_get_value(
  minsk_variable_map_t * map,
  minsk_variable_symbol_t key,
  minsk_object_t * out_value
);

bool
minsk_variable_map_find_by_name(
  minsk_variable_map_t * map,
  string_t name,
  minsk_variable_symbol_t * out_key
);

void
minsk_variable_map_del(minsk_variable_map_t * map, minsk_variable_symbol_t key);
