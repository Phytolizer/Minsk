#ifndef MINSK_UNARY_HPP_55416D5ACFCB4B69A7B256567E4DA453
#define MINSK_UNARY_HPP_55416D5ACFCB4B69A7B256567E4DA453

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <vector>

namespace minsk::analysis::syntax {

class node;

class unary_expression_syntax : public expression_syntax {
  syntax_token m_operator_token;
  std::unique_ptr<expression_syntax> m_operand;

public:
  unary_expression_syntax(syntax_token &&operator_token,
                          std::unique_ptr<expression_syntax> operand);

  [[nodiscard]] syntax_kind kind() const override;
  [[nodiscard]] std::vector<const node *> children() const override;

  [[nodiscard]] const syntax_token &operator_token() const;
  [[nodiscard]] const expression_syntax *operand() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_UNARY_HPP_55416D5ACFCB4B69A7B256567E4DA453
