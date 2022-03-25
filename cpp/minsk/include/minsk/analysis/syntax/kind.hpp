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
  bang_token,
  ampersand_ampersand_token,
  pipe_pipe_token,
  equals_equals_token,
  bang_equals_token,
  equals_token,
  open_parenthesis_token,
  close_parenthesis_token,
  open_brace_token,
  close_brace_token,

  true_keyword,
  false_keyword,
  let_keyword,
  var_keyword,

  compilation_unit,

  block_statement,
  expression_statement,
  variable_declaration,

  assignment_expression,
  binary_expression,
  literal_expression,
  name_expression,
  parenthesized_expression,
  unary_expression,
};

} // namespace minsk::analysis::syntax

#endif // MINSK_KIND_HPP
