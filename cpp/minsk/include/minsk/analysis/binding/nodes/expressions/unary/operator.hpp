#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_OPERATOR_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_OPERATOR_HPP

#include "minsk/analysis/binding/nodes/expressions/unary/kind.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/runtime/object.hpp"
namespace minsk::analysis::binding {

class bound_unary_operator final {
  syntax::syntax_kind m_syntax_kind;
  bound_unary_operator_kind m_kind;
  runtime::object_kind m_operand_type;
  runtime::object_kind m_result_type;

public:
  constexpr bound_unary_operator(syntax::syntax_kind syntax_kind,
                                 bound_unary_operator_kind kind,
                                 runtime::object_kind operand_type,
                                 runtime::object_kind result_type)
      : m_syntax_kind(syntax_kind), m_kind(kind), m_operand_type(operand_type),
        m_result_type(result_type) {}

  constexpr syntax::syntax_kind syntax_kind() const { return m_syntax_kind; }
  constexpr bound_unary_operator_kind kind() const { return m_kind; }
  constexpr runtime::object_kind operand_type() const { return m_operand_type; }
  constexpr runtime::object_kind result_type() const { return m_result_type; }

  static const bound_unary_operator *bind(syntax::syntax_kind syntax_kind,
                                          runtime::object_kind operand_type);
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_OPERATOR_HPP
