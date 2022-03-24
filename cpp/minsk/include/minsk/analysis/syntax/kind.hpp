#ifndef MINSK_KIND_HPP
#define MINSK_KIND_HPP

#include <magic_enum.hpp>

namespace minsk::analysis::syntax {

enum class syntax_kind {
  bad_token,
  end_of_file_token,

  whitespace_token,
  identifier_token,
  number_token,

  plus_token,
  minus_token,
  star_token,
  slash_token,
  open_parenthesis_token,
  close_parenthesis_token,

  true_keyword,
  false_keyword,

  binary_expression,
  literal_expression,
  parenthesized_expression,
  unary_expression,
};

} // namespace minsk::analysis::syntax

#endif // MINSK_KIND_HPP
