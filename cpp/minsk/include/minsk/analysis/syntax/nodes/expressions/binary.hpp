#ifndef MINSK_BINARY_HPP
#define MINSK_BINARY_HPP

#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>

namespace minsk::analysis::syntax {

class binary_expression_syntax : public expression_syntax {
  std::unique_ptr<expression_syntax> m_left;
  syntax_token m_operator_token;
  std::unique_ptr<expression_syntax> m_right;

public:
  binary_expression_syntax(std::unique_ptr<expression_syntax> left,
                           syntax_token &&operator_token,
                           std::unique_ptr<expression_syntax> right);

  [[nodiscard]] syntax_kind kind() const override;
  [[nodiscard]] std::vector<const node *> children() const override;

  [[nodiscard]] const expression_syntax *left() const;
  [[nodiscard]] const syntax_token &operator_token() const;
  [[nodiscard]] const expression_syntax *right() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_BINARY_HPP
