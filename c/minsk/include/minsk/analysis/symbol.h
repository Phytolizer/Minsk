#pragma once

#include "minsk/runtime/object.h"
#include "sds.h"

typedef struct {
  sds name;
  object_kind_t type;
} variable_symbol_t;

void variable_symbol_init(
    variable_symbol_t* symbol,
    sds name,
    object_kind_t type
);
void variable_symbol_free(variable_symbol_t* symbol);
variable_symbol_t variable_symbol_copy(const variable_symbol_t* symbol);
