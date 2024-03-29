#pragma once

#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/meta/concat.h"
#include "minsk/runtime/object.h"

#define MINSK_PREFIX_BOUND_EXPRESSION_BINARY_OPERATOR_KIND \
  MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_

typedef enum
{

#define X(x) \
  MINSK_CONCAT(MINSK_PREFIX_BOUND_EXPRESSION_BINARY_OPERATOR_KIND, x),
#include "minsk/code_analysis/binding/ast/expressions/private/binary_operator_kinds.xmacro"
#undef X
} minsk_bound_expression_binary_operator_kind_t;

typedef struct
{
  minsk_syntax_kind_t syntax_kind;
  minsk_bound_expression_binary_operator_kind_t kind;
  minsk_object_type_t left_type;
  minsk_object_type_t right_type;
  minsk_object_type_t result_type;
} minsk_bound_expression_binary_operator_t;

extern minsk_bound_expression_binary_operator_t const *
minsk_bound_expression_binary_operator_bind(
  minsk_syntax_kind_t syntax_kind,
  minsk_object_type_t left_type,
  minsk_object_type_t right_type
);
