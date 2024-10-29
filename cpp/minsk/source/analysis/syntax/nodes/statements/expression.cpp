#include "minsk/analysis/syntax/nodes/statements/expression.hpp"

using minsk::analysis::syntax::expression_statement_syntax;
using minsk::analysis::syntax::expression_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;

expression_statement_syntax::expression_statement_syntax(
    std::unique_ptr<expression_syntax> expression)
    : m_expression(std::move(expression)) {}

syntax_kind expression_statement_syntax::kind() const {
  return syntax_kind::expression_statement;
}

std::vector<const syntax_node *> expression_statement_syntax::children() const {
  return {
      m_expression.get(),
  };
}

const expression_syntax *expression_statement_syntax::expression() const {
  return m_expression.get();
}
