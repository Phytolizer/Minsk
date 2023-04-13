#pragma once

#include <arena.h>

#include "minsk/code_analysis/binding/ast/node.h"
#include "minsk/code_analysis/diagnostic_buf.h"
#include "minsk/code_analysis/syntax/ast/node.h"

typedef struct
{
  Arena * _arena;
  minsk_diagnostic_buf_t diagnostics;
} minsk_binder_t;

extern minsk_binder_t
minsk_binder_new(Arena * arena);
extern minsk_bound_node_t
minsk_binder_bind_expression(
  minsk_binder_t * binder,
  minsk_syntax_node_t syntax
);
