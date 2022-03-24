#include "minsk/analysis/binding/nodes/expressions/unary/operator.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/kind.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/runtime/object.hpp"

namespace binding = minsk::analysis::binding;
namespace syntax = minsk::analysis::syntax;
namespace runtime = minsk::runtime;

constexpr std::array operators = {
    binding::bound_unary_operator{
        syntax::syntax_kind::plus_token,
        binding::bound_unary_operator_kind::identity,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
    },
    binding::bound_unary_operator{
        syntax::syntax_kind::minus_token,
        binding::bound_unary_operator_kind::negation,
        runtime::object_kind::integer,
        runtime::object_kind::integer,
    },
};

const binding::bound_unary_operator *
binding::bound_unary_operator::bind(syntax::syntax_kind syntax_kind,
                                    runtime::object_kind operand_type) {
  for (const bound_unary_operator &op : operators) {
    if (op.syntax_kind() == syntax_kind && op.operand_type() == operand_type) {
      return &op;
    }
  }
  return nullptr;
}
