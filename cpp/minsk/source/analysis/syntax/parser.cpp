#include "minsk/analysis/syntax/parser.hpp"
#include "minsk/analysis/syntax/lexer.hpp"
#include <algorithm>

minsk::analysis::syntax::parser::parser(std::string_view text) {
  lexer lex{text};
  std::copy(lex.begin(), lex.end(), std::back_inserter(m_tokens));
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::parser::peek(int offset) const {
  int index = m_position + offset;
  if (index >= m_tokens.size()) {
    // return end of file token
    return m_tokens[m_tokens.size() - 1];
  } else {
    return m_tokens[index];
  }
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::parser::current() const {
  return peek(0);
}
minsk::analysis::syntax::syntax_token
minsk::analysis::syntax::parser::next_token() {
  syntax_token curr = current();
  m_position += 1;
  return curr;
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_expression() {
  return nullptr;
}
