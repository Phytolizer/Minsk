#pragma once

#include "kind.h"
#include <stdbool.h>

typedef struct {
  syntax_kind_t kind;
  bool is_token;
} syntax_node_t;

typedef struct {
  const syntax_node_t **data;
  size_t length;
} syntax_node_children_t;

void syntax_node_pretty_print(const syntax_node_t *node, FILE *stream);
syntax_node_children_t syntax_node_children(const syntax_node_t *node);
void syntax_node_free(syntax_node_t *node);
void syntax_node_children_free(syntax_node_children_t *children);
