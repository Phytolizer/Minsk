#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_OPERATOR_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_OPERATOR_HPP

#include "minsk/analysis/binding/nodes/expressions/binary/kind.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/runtime/object.hpp"
#include <array>
namespace minsk::analysis::binding {

class bound_binary_operator final {
  syntax::syntax_kind m_syntax_kind;
  bound_binary_operator_kind m_kind;
  runtime::object_kind m_left_type;
  runtime::object_kind m_right_type;
  runtime::object_kind m_result_type;

public:
  constexpr bound_binary_operator(syntax::syntax_kind syntax_kind,
                                  bound_binary_operator_kind kind,
                                  runtime::object_kind left_type,
                                  runtime::object_kind right_type,
                                  runtime::object_kind result_type)
      : m_syntax_kind(syntax_kind), m_kind(kind), m_left_type(left_type),
        m_right_type(right_type), m_result_type(result_type) {}

  static const bound_binary_operator *bind(syntax::syntax_kind syntax_kind,
                                           runtime::object_kind left_type,
                                           runtime::object_kind right_type);
  constexpr syntax::syntax_kind syntax_kind() const { return m_syntax_kind; }
  constexpr bound_binary_operator_kind kind() const { return m_kind; }
  constexpr runtime::object_kind left_type() const { return m_left_type; }
  constexpr runtime::object_kind right_type() const { return m_right_type; }
  constexpr runtime::object_kind result_type() const { return m_result_type; }
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_OPERATOR_HPP
