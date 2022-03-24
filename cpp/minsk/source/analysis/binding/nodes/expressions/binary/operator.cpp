#include "minsk/analysis/binding/nodes/expressions/binary/operator.hpp"

namespace syntax = minsk::analysis::syntax;
namespace binding = minsk::analysis::binding;
namespace runtime = minsk::runtime;

constexpr std::array operators = {
    binding::bound_binary_operator{
        syntax::syntax_kind::plus_token,
        binding::bound_binary_operator_kind::addition,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
    },
    binding::bound_binary_operator{
        syntax::syntax_kind::minus_token,
        binding::bound_binary_operator_kind::subtraction,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
    },
    binding::bound_binary_operator{
        syntax::syntax_kind::star_token,
        binding::bound_binary_operator_kind::multiplication,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
    },
    binding::bound_binary_operator{
        syntax::syntax_kind::slash_token,
        binding::bound_binary_operator_kind::division,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
    },
};

const binding::bound_binary_operator *
minsk::analysis::binding::bound_binary_operator::bind(
    syntax::syntax_kind syntax_kind, runtime::object_kind left_type,
    runtime::object_kind right_type) {
  for (const bound_binary_operator &op : operators) {
    if (op.syntax_kind() == syntax_kind && op.left_type() == left_type &&
        op.right_type() == right_type) {
      return &op;
    }
  }
  return nullptr;
}
