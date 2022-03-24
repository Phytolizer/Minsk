#ifndef MINSK_TOKEN_HPP
#define MINSK_TOKEN_HPP

#include "kind.hpp"
#include "minsk/runtime/object.hpp"
#include <string>
#include <string_view>

namespace minsk::analysis::syntax {

class syntax_token {
  syntax_kind m_kind;
  int m_position;
  std::string m_text;
  runtime::object_ptr m_value;

public:
  syntax_token(syntax_kind kind, int position, std::string text,
        runtime::object_ptr value);
  syntax_token(const syntax_token &other);
  syntax_token& operator=(const syntax_token& other);

  [[nodiscard]] syntax_kind kind() const;
  [[nodiscard]] int position() const;
  [[nodiscard]] std::string_view text() const;
  [[nodiscard]] runtime::object* value() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_TOKEN_HPP
