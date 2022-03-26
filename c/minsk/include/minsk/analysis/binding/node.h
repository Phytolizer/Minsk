#pragma once

#include "kind.h"

typedef struct {
  bound_node_kind_t kind;
} bound_node_t;

void bound_node_free(bound_node_t *node);
