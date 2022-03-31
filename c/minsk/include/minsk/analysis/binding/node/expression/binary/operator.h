#pragma once

#include "kind.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/runtime/object.h"

typedef struct {
  syntax_kind_t syntax_kind;
  bound_binary_operator_kind_t kind;
  object_kind_t left_type;
  object_kind_t right_type;
  object_kind_t result_type;
} bound_binary_operator_t;

const bound_binary_operator_t* bound_binary_operator_bind(
    syntax_kind_t syntax_kind, object_kind_t left_type,
    object_kind_t right_type);
