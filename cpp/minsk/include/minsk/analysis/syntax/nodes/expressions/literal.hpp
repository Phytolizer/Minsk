#ifndef MINSK_LITERAL_HPP
#define MINSK_LITERAL_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/runtime/object.hpp"
#include <vector>

namespace minsk::analysis::syntax {

class node;

class literal_expression_syntax final : public expression_syntax {
  syntax_token m_literal_token;
  runtime::object_ptr m_value;

public:
  explicit literal_expression_syntax(syntax_token &&literal_token);
  literal_expression_syntax(syntax_token &&literal_token,
                            runtime::object_ptr value);

  [[nodiscard]] syntax_kind kind() const override;
  [[nodiscard]] std::vector<const node *> children() const override;

  [[nodiscard]] const syntax_token &literal_token() const;
  [[nodiscard]] const runtime::object *value() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_LITERAL_HPP
