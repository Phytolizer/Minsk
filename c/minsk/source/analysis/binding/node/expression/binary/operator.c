#include "minsk/analysis/binding/node/expression/binary/operator.h"
#include "minsk/analysis/binding/node/expression/binary/kind.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/runtime/object.h"

static const bound_binary_operator_t operators[] = {
    {
        syntax_kind_plus_token,
        bound_binary_operator_kind_addition,
        object_kind_integer,
        object_kind_integer,
        object_kind_integer,
    },
    {
        syntax_kind_minus_token,
        bound_binary_operator_kind_subtraction,
        object_kind_integer,
        object_kind_integer,
        object_kind_integer,
    },
    {
        syntax_kind_star_token,
        bound_binary_operator_kind_multiplication,
        object_kind_integer,
        object_kind_integer,
        object_kind_integer,
    },
    {
        syntax_kind_slash_token,
        bound_binary_operator_kind_division,
        object_kind_integer,
        object_kind_integer,
        object_kind_integer,
    },
};

const bound_binary_operator_t *
bound_binary_operator_bind(syntax_kind_t syntax_kind, object_kind_t left_type,
                           object_kind_t right_type) {
  for (size_t i = 0; i < sizeof(operators) / sizeof(operators[0]); i++) {
    const bound_binary_operator_t *op = &operators[i];
    if (op->syntax_kind == syntax_kind && op->left_type == left_type &&
        op->right_type == right_type) {
      return op;
    }
  }
  return NULL;
}
