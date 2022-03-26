#pragma once

#include "kind.h"
#include "minsk/runtime/object.h"
#include "node.h"
#include "sds.h"
#include <stdio.h>

typedef struct {
  syntax_node_t base;
  int position;
  sds text;
  object_t *value;
} syntax_token_t;

void token_print(syntax_token_t *token, FILE *stream);
void token_free(syntax_token_t *token);
