#pragma once

#include "minsk/analysis/binding/node/expression/unary/kind.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/runtime/object.h"

typedef struct {
  syntax_kind_t syntax_kind;
  bound_unary_operator_kind_t kind;
  object_kind_t operand_type;
  object_kind_t result_type;
} bound_unary_operator_t;

const bound_unary_operator_t* bound_unary_operator_bind(
    syntax_kind_t syntax_kind,
    object_kind_t operand_type
);
