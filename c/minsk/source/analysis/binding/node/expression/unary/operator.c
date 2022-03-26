#include "minsk/analysis/binding/node/expression/unary/operator.h"
#include "minsk/analysis/binding/node/expression/unary/kind.h"
#include "minsk/analysis/syntax/kind.h"

static const bound_unary_operator_t operators[] = {
    {
        .syntax_kind = syntax_kind_plus_token,
        .kind = bound_unary_operator_kind_identity,
        .operand_type = object_kind_integer,
        .result_type = object_kind_integer,
    },
    {
        .syntax_kind = syntax_kind_minus_token,
        .kind = bound_unary_operator_kind_negation,
        .operand_type = object_kind_integer,
        .result_type = object_kind_integer,
    },
    {
        .syntax_kind = syntax_kind_bang_token,
        .kind = bound_unary_operator_kind_logical_negation,
        .operand_type = object_kind_boolean,
        .result_type = object_kind_boolean,
    },
};

const bound_unary_operator_t *
bound_unary_operator_bind(syntax_kind_t syntax_kind,
                          object_kind_t operand_type) {
  for (size_t i = 0; i < sizeof(operators) / sizeof(operators[0]); i++) {
    const bound_unary_operator_t *op = &operators[i];
    if (op->syntax_kind == syntax_kind && op->operand_type == operand_type) {
      return op;
    }
  }
  return NULL;
}
