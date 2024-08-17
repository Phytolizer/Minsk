#include "minsk/code_analysis/syntax/parser.h"

#include <arena.h>
#include <minsk-string/string.h>
#include <stdbool.h>

#include "minsk/code_analysis/syntax/ast/expression.h"
#include "minsk/code_analysis/syntax/ast/node.h"
#include "minsk/code_analysis/syntax/facts.h"
#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/code_analysis/syntax/lexer.h"
#include "minsk/code_analysis/syntax/token.h"
#include "minsk/runtime/object.h"

static minsk_syntax_token_t
peek(minsk_syntax_parser_t const * parser, int offset)
{
  size_t index = parser->_position + offset;
  if (index >= parser->_tokens.len)
  {
    return parser->_tokens.ptr[parser->_tokens.len - 1];
  }
  return parser->_tokens.ptr[index];
}

static inline minsk_syntax_token_t
current(minsk_syntax_parser_t const * parser)
{
  return peek(parser, 0);
}

static minsk_syntax_token_t
next_token(minsk_syntax_parser_t * parser)
{
  minsk_syntax_token_t tok = current(parser);
  ++parser->_position;
  return tok;
}

static minsk_syntax_token_t
match_token(minsk_syntax_parser_t * parser, minsk_syntax_kind_t kind)
{
  if (current(parser).kind == kind)
  {
    return next_token(parser);
  }

  minsk_diagnostic_bag_report_unexpected_token(
    &parser->diagnostics,
    minsk_syntax_token_span(current(parser)),
    current(parser).kind,
    kind
  );
  return (minsk_syntax_token_t){
    .kind = kind,
    .position = current(parser).position,
    .text = EMPTY_STRING,
    .value = MINSK_OBJECT_NIL,
  };
}

extern minsk_syntax_parser_t
minsk_syntax_parser_new(Arena * arena, minsk_text_source_text_t text)
{
  minsk_syntax_lexer_t lexer = minsk_syntax_lexer_new(arena, text);
  minsk_syntax_parser_token_buf_t tokens = BUF_INIT;
  while (true)
  {
    minsk_syntax_token_t tok = minsk_syntax_lexer_lex(&lexer);
    if (tok.kind == MINSK_SYNTAX_KIND_WHITESPACE_TOKEN ||
        tok.kind == MINSK_SYNTAX_KIND_BAD_TOKEN)
    {
      string_free(tok.text);
      continue;
    }
    BUF_PUSH_ARENA(arena, &tokens, tok);
    if (tok.kind == MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN)
    {
      break;
    }
  }
  return (minsk_syntax_parser_t){
    ._arena = arena,
    ._text = text,
    ._tokens = tokens,
    ._position = 0,
    .diagnostics = lexer.diagnostics,
  };
}

static minsk_syntax_node_t
parse_expression(minsk_syntax_parser_t * parser);
static minsk_syntax_node_t
parse_assignment_expression(minsk_syntax_parser_t * parser);
static minsk_syntax_node_t
parse_binary_expression(
  minsk_syntax_parser_t * parser,
  minsk_syntax_facts_precedence_t precedence
);
static minsk_syntax_node_t
parse_primary_expression(minsk_syntax_parser_t * parser);

extern minsk_syntax_compilation_unit_t
minsk_syntax_parser_parse_compilation_unit(minsk_syntax_parser_t * parser)
{
  minsk_syntax_node_t expression = parse_expression(parser);
  minsk_syntax_token_t end_of_file_token =
    match_token(parser, MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN);
  return (minsk_syntax_compilation_unit_t){
    .expression = minsk_syntax_node_promote(parser->_arena, expression),
    .end_of_file_token = end_of_file_token,
  };
}

static minsk_syntax_node_t
parse_expression(minsk_syntax_parser_t * parser)
{
  return parse_assignment_expression(parser);
}

inline minsk_syntax_node_t
parse_assignment_expression(minsk_syntax_parser_t * parser)
{
  if (peek(parser, 0).kind == MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN &&
      peek(parser, 1).kind == MINSK_SYNTAX_KIND_EQUALS_TOKEN)
  {
    minsk_syntax_token_t identifier_token = next_token(parser);
    minsk_syntax_token_t equals_token = next_token(parser);
    minsk_syntax_node_t expression = parse_assignment_expression(parser);
    return MINSK_SYNTAX_EXPRESSION_ASSIGNMENT(
        .identifier_token = identifier_token,
        .equals_token = equals_token,
        .expression = minsk_syntax_node_promote(parser->_arena, expression)
    );
  }

  return parse_binary_expression(parser, 0);
}

static minsk_syntax_node_t
parse_binary_expression(
  minsk_syntax_parser_t * parser,
  minsk_syntax_facts_precedence_t parent_precedence
)
{
  minsk_syntax_node_t left;
  minsk_syntax_facts_precedence_t unary_precedence =
    minsk_syntax_facts_unary_operator_precedence(current(parser).kind);
  if (unary_precedence != 0 && unary_precedence >= parent_precedence)
  {
    minsk_syntax_token_t op = next_token(parser);
    minsk_syntax_node_t operand =
      parse_binary_expression(parser, unary_precedence);
    left = MINSK_SYNTAX_EXPRESSION_UNARY(
        .op = op,
        .operand = minsk_syntax_node_promote(parser->_arena, operand)
    );
  }
  else
  {
    left = parse_primary_expression(parser);
  }

  while (true)
  {
    minsk_syntax_facts_precedence_t precedence =
      minsk_syntax_facts_binary_operator_precedence(current(parser).kind);
    if (precedence == 0 || precedence <= parent_precedence)
    {
      break;
    }

    minsk_syntax_token_t op = next_token(parser);
    minsk_syntax_node_t * right = minsk_syntax_node_promote(
      parser->_arena,
      parse_binary_expression(parser, precedence)
    );
    minsk_syntax_node_t * left_alloc =
      minsk_syntax_node_promote(parser->_arena, left);
    left = MINSK_SYNTAX_EXPRESSION_BINARY(
        .left = left_alloc,
        .op = op,
        .right = right
    );
  }

  return left;
}

static minsk_syntax_node_t
parse_parenthesized_expression(minsk_syntax_parser_t * parser);
static minsk_syntax_node_t
parse_boolean_literal(minsk_syntax_parser_t * parser);
static minsk_syntax_node_t
parse_number_literal(minsk_syntax_parser_t * parser);
static minsk_syntax_node_t
parse_name_expression(minsk_syntax_parser_t * parser);

static minsk_syntax_node_t
parse_primary_expression(minsk_syntax_parser_t * parser)
{
  switch (current(parser).kind)
  {
    case MINSK_SYNTAX_KIND_OPEN_PARENTHESIS_TOKEN:
      return parse_parenthesized_expression(parser);
    case MINSK_SYNTAX_KIND_TRUE_KEYWORD:
    case MINSK_SYNTAX_KIND_FALSE_KEYWORD: return parse_boolean_literal(parser);
    case MINSK_SYNTAX_KIND_NUMBER_TOKEN: return parse_number_literal(parser);
    default: return parse_name_expression(parser);
  }
}

static minsk_syntax_node_t
parse_parenthesized_expression(minsk_syntax_parser_t * parser)
{
  minsk_syntax_token_t open_parenthesis_token =
    match_token(parser, MINSK_SYNTAX_KIND_OPEN_PARENTHESIS_TOKEN);
  minsk_syntax_node_t expression = parse_expression(parser);
  minsk_syntax_token_t close_parenthesis_token =
    match_token(parser, MINSK_SYNTAX_KIND_CLOSE_PARENTHESIS_TOKEN);
  return MINSK_SYNTAX_EXPRESSION_PARENTHESIZED(
      .open_parenthesis_token = open_parenthesis_token,
      .expression = minsk_syntax_node_promote(parser->_arena, expression),
      .close_parenthesis_token = close_parenthesis_token
  );
}

static minsk_syntax_node_t
parse_boolean_literal(minsk_syntax_parser_t * parser)
{
  bool value = current(parser).kind == MINSK_SYNTAX_KIND_TRUE_KEYWORD;
  minsk_syntax_token_t keyword_token =
    value ? next_token(parser)
          : match_token(parser, MINSK_SYNTAX_KIND_FALSE_KEYWORD);
  return MINSK_SYNTAX_EXPRESSION_LITERAL(
      .literal_token = keyword_token,
      .value = MINSK_OBJECT_BOOLEAN(value)
  );
}

static minsk_syntax_node_t
parse_number_literal(minsk_syntax_parser_t * parser)
{
  minsk_syntax_token_t literal_token =
    match_token(parser, MINSK_SYNTAX_KIND_NUMBER_TOKEN);
  return MINSK_SYNTAX_EXPRESSION_LITERAL(
      .literal_token = literal_token,
      .value = literal_token.value
  );
}

inline minsk_syntax_node_t
parse_name_expression(minsk_syntax_parser_t * parser)
{
  minsk_syntax_token_t identifier_token =
    match_token(parser, MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN);
  return MINSK_SYNTAX_EXPRESSION_NAME(.identifier_token = identifier_token);
}
