#ifndef MINSK_PARENTHESIZED_HPP_9D5EFCC0FC5C4FB493D8550246C72DD8
#define MINSK_PARENTHESIZED_HPP_9D5EFCC0FC5C4FB493D8550246C72DD8

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <vector>

namespace minsk::analysis::syntax {

class node;

class parenthesized_expression_syntax final : public expression_syntax {
  syntax_token m_open_parenthesis_token;
  std::unique_ptr<expression_syntax> m_expression;
  syntax_token m_close_parenthesis_token;

public:
  parenthesized_expression_syntax(syntax_token &&open_parenthesis_token,
                                  std::unique_ptr<expression_syntax> expression,
                                  syntax_token &&close_parenthesis_token);

  [[nodiscard]] syntax_kind kind() const override;
  [[nodiscard]] std::vector<const node *> children() const override;

  [[nodiscard]] const syntax_token &open_parenthesis_token() const;
  [[nodiscard]] const expression_syntax *expression() const;
  [[nodiscard]] const syntax_token &close_parenthesis_token() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_PARENTHESIZED_HPP_9D5EFCC0FC5C4FB493D8550246C72DD8
