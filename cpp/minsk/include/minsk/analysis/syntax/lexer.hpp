#ifndef MINSK_LEXER_HPP
#define MINSK_LEXER_HPP

#include "token.hpp"
#include <concepts>

namespace minsk::analysis::syntax {

class lexer {
  std::string_view m_text;

public:
  class iterator {
    const lexer *m_lexer;
    int m_position;
    syntax_token just_scanned;

    syntax_token scan();
    [[nodiscard]] char peek(int offset) const;
    [[nodiscard]] char current() const;
    [[nodiscard]] std::string current_text(int start) const;

  public:
    explicit iterator(const lexer *lex);
    iterator();

    [[nodiscard]] const syntax_token &operator*() const;
    iterator &operator++();
    iterator operator++(int);
    [[nodiscard]] bool operator==(const iterator &other) const;
  };

  explicit lexer(std::string_view text);

  iterator begin() const;
  iterator end() const;
};

} // namespace minsk::analysis::syntax

namespace std {
template <> struct iterator_traits<minsk::analysis::syntax::lexer::iterator> {
  using value_type = minsk::analysis::syntax::syntax_token;
  using difference_type = int;
  using iterator_category = std::input_iterator_tag;
};
} // namespace std

static_assert(std::input_iterator<minsk::analysis::syntax::lexer::iterator>);

#endif // MINSK_LEXER_HPP
