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
  tilde_token,
  ampersand_ampersand_token,
  pipe_pipe_token,
  ampersand_token,
  pipe_token,
  hat_token,
  equals_equals_token,
  bang_equals_token,
  less_token,
  less_equals_token,
  greater_token,
  greater_equals_token,
  equals_token,
  open_parenthesis_token,
  close_parenthesis_token,
  open_brace_token,
  close_brace_token,

  true_keyword,
  false_keyword,
  let_keyword,
  var_keyword,
  if_keyword,
  else_keyword,
  while_keyword,
  for_keyword,
  to_keyword,

  compilation_unit,
  else_clause,

  block_statement,
  expression_statement,
  variable_declaration,
  if_statement,
  while_statement,
  for_statement,

  assignment_expression,
  binary_expression,
  literal_expression,
  name_expression,
  parenthesized_expression,
  unary_expression,
};

} // namespace minsk::analysis::syntax

#endif // MINSK_KIND_HPP
