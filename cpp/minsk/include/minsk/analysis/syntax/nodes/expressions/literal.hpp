#ifndef MINSK_LITERAL_HPP
#define MINSK_LITERAL_HPP

#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"

namespace minsk::analysis::syntax {

class literal_expression_syntax : public expression_syntax {
  syntax_token m_literal_token;

public:
  explicit literal_expression_syntax(syntax_token &&literal_token);

  [[nodiscard]] syntax_kind kind() const override;
  [[nodiscard]] std::vector<const node *> children() const override;

  [[nodiscard]] const syntax_token &literal_token() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_LITERAL_HPP
