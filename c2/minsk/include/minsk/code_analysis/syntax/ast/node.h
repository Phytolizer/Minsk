#pragma once

#include <arena.h>
#include <stdio.h>

#include "minsk/code_analysis/syntax/ast/expression.h"
#include "minsk/code_analysis/syntax/ast/node_type.h"
#include "minsk/code_analysis/syntax/token.h"
#include "minsk/code_analysis/text/span.h"
#include "minsk/data_structures/buf.h"

typedef struct minsk_syntax_node
{
  minsk_syntax_node_type_t type;

  union
  {
    minsk_syntax_token_t token;
    minsk_syntax_expression_t expression;
  };
} minsk_syntax_node_t;

#define MINSK_SYNTAX_NODE_TOKEN(tok) \
  ((minsk_syntax_node_t){.type = MINSK_SYNTAX_NODE_TYPE_TOKEN, .token = (tok)})

typedef BUF_T(minsk_syntax_node_t) minsk_syntax_node_buf_t;

extern minsk_syntax_node_t *
minsk_syntax_node_promote(Arena * arena, minsk_syntax_node_t node);

extern void
minsk_syntax_node_pretty_print(minsk_syntax_node_t node, FILE * stream);

extern minsk_syntax_node_buf_t
minsk_syntax_node_children(Arena * arena, minsk_syntax_node_t node);

extern minsk_text_span_t
minsk_syntax_node_span(Arena * arena, minsk_syntax_node_t node);
