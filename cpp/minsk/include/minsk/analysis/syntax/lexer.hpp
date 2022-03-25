#ifndef MINSK_LEXER_HPP
#define MINSK_LEXER_HPP

#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/text/source.hpp"
#include "token.hpp"
#include <concepts>
#include <ranges>

namespace minsk::analysis::syntax {

class lexer final {
  const text::source_text *m_text;
  diagnostic_bag m_diagnostics;

public:
  class iterator {
    lexer *m_lexer;
    int m_position;
    bool m_at_end;
    syntax_token m_just_scanned;

    syntax_token scan();
    [[nodiscard]] char peek(int offset) const;
    [[nodiscard]] char current() const;
    [[nodiscard]] std::string current_text(int start) const;

  public:
    explicit iterator(lexer *lex);
    iterator();

    [[nodiscard]] const syntax_token &operator*() const;
    iterator &operator++();
    iterator operator++(int);
    [[nodiscard]] bool operator==(const iterator &other) const;
  };

  explicit lexer(const text::source_text *text);

  iterator begin();
  iterator end();
  diagnostic_bag &diagnostics();
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
