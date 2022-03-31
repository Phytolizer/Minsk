#include "minsk/analysis/symbol.h"

void variable_symbol_init(
    variable_symbol_t* symbol, sds name, object_kind_t type) {
  symbol->name = name;
  symbol->type = type;
}

void variable_symbol_free(variable_symbol_t* symbol) {
  sdsfree(symbol->name);
}

variable_symbol_t variable_symbol_copy(const variable_symbol_t* symbol) {
  return (variable_symbol_t){
      .name = sdsdup(symbol->name),
      .type = symbol->type,
  };
}
