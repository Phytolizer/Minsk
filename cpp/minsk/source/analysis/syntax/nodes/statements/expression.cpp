#include "minsk/analysis/syntax/nodes/statements/expression.hpp"

minsk::analysis::syntax::expression_statement_syntax::
    expression_statement_syntax(std::unique_ptr<expression_syntax> expression)
    : m_expression(std::move(expression)) {}

minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::expression_statement_syntax::kind() const {
  return syntax_kind::expression_statement;
}

std::vector<const minsk::analysis::syntax::syntax_node *>
minsk::analysis::syntax::expression_statement_syntax::children() const {
  return {
      m_expression.get(),
  };
}

const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::expression_statement_syntax::expression() const {
  return m_expression.get();
}
