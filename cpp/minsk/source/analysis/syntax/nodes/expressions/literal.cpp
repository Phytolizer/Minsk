#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/runtime/object.hpp"
#include <utility>

using minsk::analysis::syntax::literal_expression_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;
using minsk::runtime::object;

literal_expression_syntax::literal_expression_syntax(
    syntax_token &&literal_token)
    : m_literal_token(std::move(literal_token)),
      m_value(runtime::copy_object_ptr(m_literal_token.value())) {}

literal_expression_syntax::literal_expression_syntax(
    syntax_token &&literal_token, runtime::object_ptr value)
    : m_literal_token(std::move(literal_token)), m_value(std::move(value)) {}

syntax_kind literal_expression_syntax::kind() const {
  return syntax_kind::literal_expression;
}

std::vector<const syntax_node *> literal_expression_syntax::children() const {
  return {
      &m_literal_token,
  };
}

const syntax_token &literal_expression_syntax::literal_token() const {
  return m_literal_token;
}

const object *literal_expression_syntax::value() const { return m_value.get(); }
