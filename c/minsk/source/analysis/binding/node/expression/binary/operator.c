#include "minsk/analysis/binding/node/expression/binary/operator.h"
#include "minsk/analysis/binding/node/expression/binary/kind.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/runtime/object.h"

static const bound_binary_operator_t operators[] = {
    {
        .syntax_kind = syntax_kind_plus_token,
        .kind = bound_binary_operator_kind_addition,
        .left_type = object_kind_integer,
        .right_type = object_kind_integer,
        .result_type = object_kind_integer,
    },
    {
        .syntax_kind = syntax_kind_minus_token,
        .kind = bound_binary_operator_kind_subtraction,
        .left_type = object_kind_integer,
        .right_type = object_kind_integer,
        .result_type = object_kind_integer,
    },
    {
        .syntax_kind = syntax_kind_star_token,
        .kind = bound_binary_operator_kind_multiplication,
        .left_type = object_kind_integer,
        .right_type = object_kind_integer,
        .result_type = object_kind_integer,
    },
    {
        .syntax_kind = syntax_kind_slash_token,
        .kind = bound_binary_operator_kind_division,
        .left_type = object_kind_integer,
        .right_type = object_kind_integer,
        .result_type = object_kind_integer,
    },
    {
        .syntax_kind = syntax_kind_ampersand_ampersand_token,
        .kind = bound_binary_operator_kind_logical_and,
        .left_type = object_kind_boolean,
        .right_type = object_kind_boolean,
        .result_type = object_kind_boolean,
    },
    {
        .syntax_kind = syntax_kind_pipe_pipe_token,
        .kind = bound_binary_operator_kind_logical_or,
        .left_type = object_kind_boolean,
        .right_type = object_kind_boolean,
        .result_type = object_kind_boolean,
    },
    {
        .syntax_kind = syntax_kind_equals_equals_token,
        .kind = bound_binary_operator_kind_equality,
        .left_type = object_kind_boolean,
        .right_type = object_kind_boolean,
        .result_type = object_kind_boolean,
    },
    {
        .syntax_kind = syntax_kind_bang_equals_token,
        .kind = bound_binary_operator_kind_inequality,
        .left_type = object_kind_boolean,
        .right_type = object_kind_boolean,
        .result_type = object_kind_boolean,
    },
    {
        .syntax_kind = syntax_kind_equals_equals_token,
        .kind = bound_binary_operator_kind_equality,
        .left_type = object_kind_integer,
        .right_type = object_kind_integer,
        .result_type = object_kind_boolean,
    },
    {
        .syntax_kind = syntax_kind_bang_equals_token,
        .kind = bound_binary_operator_kind_inequality,
        .left_type = object_kind_integer,
        .right_type = object_kind_integer,
        .result_type = object_kind_boolean,
    },
};

const bound_binary_operator_t* bound_binary_operator_bind(
    syntax_kind_t syntax_kind,
    object_kind_t left_type,
    object_kind_t right_type
) {
  for (size_t i = 0; i < sizeof(operators) / sizeof(operators[0]); i++) {
    const bound_binary_operator_t* op = &operators[i];
    if (op->syntax_kind == syntax_kind && op->left_type == left_type &&
        op->right_type == right_type) {
      return op;
    }
  }
  return NULL;
}
