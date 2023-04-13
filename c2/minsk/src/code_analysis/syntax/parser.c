#include "minsk/code_analysis/syntax/parser.h"

#include <arena.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/lexer.h>
#include <minsk/code_analysis/syntax/token.h>
#include <stdbool.h>

static minsk_syntax_token_t
peek(minsk_syntax_parser_t const* parser, int offset)
{
  size_t index = parser->_position + offset;
  if (index >= parser->_tokens.len)
  {
    return parser->_tokens.ptr[parser->_tokens.len - 1];
  }
  return parser->_tokens.ptr[index];
}

static inline minsk_syntax_token_t current(minsk_syntax_parser_t const* parser)
{
  return peek(parser, 0);
}

static minsk_syntax_token_t next_token(minsk_syntax_parser_t* parser)
{
  minsk_syntax_token_t tok = current(parser);
  ++parser->_position;
  return tok;
}

static minsk_syntax_token_t
match_token(minsk_syntax_parser_t* parser, minsk_syntax_kind_t kind)
{
  if (current(parser).kind == kind)
  {
    return next_token(parser);
  }

  BUF_PUSH_ARENA(
    parser->_arena,
    &parser->diagnostics,
    string_printf_arena(
      parser->_arena,
      "Unexpected token <" STRING_FMT ">, expected <" STRING_FMT ">.",
      STRING_ARG(
        minsk_syntax_kind_display_name(parser->_arena, current(parser).kind)
      ),
      STRING_ARG(minsk_syntax_kind_display_name(parser->_arena, kind))
    )
  );
  return (minsk_syntax_token_t){
    .kind = kind,
    .position = current(parser).position,
    .text = EMPTY_STRING,
    .value = MINSK_OBJECT_NIL,
  };
}

extern minsk_syntax_parser_t
minsk_syntax_parser_new(Arena* arena, string_t text)
{
  minsk_syntax_lexer_t lexer = minsk_syntax_lexer_new(arena, text);
  minsk_syntax_parser_token_buf_t tokens = {0};
  while (true)
  {
    minsk_syntax_token_t tok = minsk_syntax_lexer_lex(&lexer);
    if (tok.kind == MINSK_SYNTAX_KIND_WHITESPACE_TOKEN || tok.kind == MINSK_SYNTAX_KIND_BAD_TOKEN)
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
    ._tokens = tokens,
    ._position = 0,
    .diagnostics = lexer.diagnostics,
  };
}

static minsk_syntax_node_t parse_expression(minsk_syntax_parser_t* parser);
static minsk_syntax_node_t parse_term(minsk_syntax_parser_t* parser);
static minsk_syntax_node_t parse_factor(minsk_syntax_parser_t* parser);
static minsk_syntax_node_t parse_primary_expression(
  minsk_syntax_parser_t* parser
);

extern minsk_syntax_tree_t minsk_syntax_parser_parse(
  minsk_syntax_parser_t* parser
)
{
  minsk_syntax_node_t expression = parse_expression(parser);
  minsk_syntax_token_t end_of_file_token =
    match_token(parser, MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN);
  return (minsk_syntax_tree_t){
    .diagnostics = parser->diagnostics,
    .root = expression,
    .end_of_file_token = end_of_file_token,
  };
}

static minsk_syntax_node_t parse_expression(minsk_syntax_parser_t* parser)
{
  return parse_term(parser);
}

static minsk_syntax_node_t parse_term(minsk_syntax_parser_t* parser)
{
  minsk_syntax_node_t left = parse_factor(parser);

  while (current(parser).kind == MINSK_SYNTAX_KIND_PLUS_TOKEN ||
         current(parser).kind == MINSK_SYNTAX_KIND_MINUS_TOKEN)
  {
    minsk_syntax_token_t op = next_token(parser);
    minsk_syntax_node_t* right =
      minsk_syntax_node_promote(parser->_arena, parse_factor(parser));
    minsk_syntax_node_t* left_alloc =
      minsk_syntax_node_promote(parser->_arena, left);
    left = MINSK_SYNTAX_EXPRESSION_BINARY(left_alloc, op, right);
  }

  return left;
}

static minsk_syntax_node_t parse_factor(minsk_syntax_parser_t* parser)
{
  minsk_syntax_node_t left = parse_primary_expression(parser);

  while (current(parser).kind == MINSK_SYNTAX_KIND_STAR_TOKEN ||
         current(parser).kind == MINSK_SYNTAX_KIND_SLASH_TOKEN)
  {
    minsk_syntax_token_t op = next_token(parser);
    minsk_syntax_node_t* right = minsk_syntax_node_promote(
      parser->_arena,
      parse_primary_expression(parser)
    );
    minsk_syntax_node_t* left_alloc =
      minsk_syntax_node_promote(parser->_arena, left);
    left = MINSK_SYNTAX_EXPRESSION_BINARY(left_alloc, op, right);
  }

  return left;
}

static minsk_syntax_node_t parse_primary_expression(
  minsk_syntax_parser_t* parser
)
{
  minsk_syntax_token_t literal_token =
    match_token(parser, MINSK_SYNTAX_KIND_NUMBER_TOKEN);
  return MINSK_SYNTAX_EXPRESSION_LITERAL(literal_token);
}
