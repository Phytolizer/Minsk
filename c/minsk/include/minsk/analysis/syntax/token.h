#pragma once

#include "kind.h"
#include "minsk/analysis/text/span.h"
#include "minsk/runtime/object.h"
#include "node.h"
#include "sds.h"
#include <stddef.h>
#include <stdio.h>

typedef struct {
  syntax_node_t base;
  size_t position;
  sds text;
  object_t* value;
} syntax_token_t;

void token_print(syntax_token_t* token, FILE* stream);
text_span_t token_span(const syntax_token_t* token);
void token_free(syntax_token_t* token);
