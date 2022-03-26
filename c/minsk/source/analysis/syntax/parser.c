#include "minsk/analysis/syntax/parser.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/facts.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/expression/binary.h"
#include "minsk/analysis/syntax/node/expression/literal.h"
#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include "minsk/analysis/syntax/node/expression/unary.h"
#include <stddef.h>

static syntax_token_t *current(parser_t *parser) {
  return peek_buffer_peek(&parser->peek_buffer, 0);
}

static syntax_token_t match_token(parser_t *parser, syntax_kind_t kind) {
  if (current(parser)->base.kind == kind) {
    return peek_buffer_pop(&parser->peek_buffer);
  }

  diagnostic_bag_report_unexpected_token(&parser->diagnostics,
                                         token_span(current(parser)), kind,
                                         current(parser)->base.kind);
  return (syntax_token_t){
      .base = {.kind = kind, .is_token = true},
      .position = current(parser)->position,
      .text = sdsempty(),
      .value = NULL,
  };
}

static expression_syntax_t *parse_expression(parser_t *parser);

static expression_syntax_t *parse_parenthesized_expression(parser_t *parser) {
  syntax_token_t open_parenthesis_token =
      match_token(parser, syntax_kind_open_parenthesis_token);
  expression_syntax_t *expression = parse_expression(parser);
  syntax_token_t close_parenthesis_token =
      match_token(parser, syntax_kind_close_parenthesis_token);
  return parenthesized_expression_syntax_new(open_parenthesis_token, expression,
                                             close_parenthesis_token);
}

static expression_syntax_t *parse_number_literal(parser_t *parser) {
  syntax_token_t number_token = match_token(parser, syntax_kind_number_token);
  return literal_expression_syntax_new(number_token, NULL);
}
static expression_syntax_t *parse_primary_expression(parser_t *parser) {
  switch (current(parser)->base.kind) {
  case syntax_kind_open_parenthesis_token:
    return parse_parenthesized_expression(parser);
  default:
    return parse_number_literal(parser);
  }
}

static expression_syntax_t *parse_binary_expression(parser_t *parser,
                                                    int parent_precedence) {
  expression_syntax_t *left;
  int unary_operator_precedence =
      facts_unary_operator_precedence(current(parser)->base.kind);
  if (unary_operator_precedence != 0 &&
      unary_operator_precedence >= parent_precedence) {
    syntax_token_t operator_token = peek_buffer_pop(&parser->peek_buffer);
    expression_syntax_t *right =
        parse_binary_expression(parser, unary_operator_precedence);
    left = unary_expression_syntax_new(operator_token, right);
  } else {
    left = parse_primary_expression(parser);
  }

  while (true) {
    int precedence =
        facts_binary_operator_precedence(current(parser)->base.kind);
    if (precedence == 0 || precedence <= parent_precedence) {
      break;
    }

    syntax_token_t operator_token = peek_buffer_pop(&parser->peek_buffer);
    expression_syntax_t *right = parse_binary_expression(parser, precedence);
    left = binary_expression_syntax_new(left, operator_token, right);
  }

  return left;
}

static expression_syntax_t *parse_expression(parser_t *parser) {
  return parse_binary_expression(parser, 0);
}

void parser_init(parser_t *parser, const char *text) {
  peek_buffer_init(&parser->peek_buffer, text);
  diagnostic_bag_init(&parser->diagnostics);
}
expression_syntax_t *parser_parse(parser_t *parser) {
  expression_syntax_t *expression = parse_expression(parser);
  diagnostic_bag_t bag;
  diagnostic_bag_init(&bag);
  for (size_t i = 0; i < parser->peek_buffer.lexer.diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(
        &bag, parser->peek_buffer.lexer.diagnostics.data[i]);
  }
  for (size_t i = 0; i < parser->diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(&bag, parser->diagnostics.data[i]);
  }
  diagnostic_bag_free(&parser->diagnostics);
  parser->diagnostics = bag;
  return expression;
}
void parser_free(parser_t *parser) {
  peek_buffer_free(&parser->peek_buffer);
  parser->peek_buffer.lexer.text = NULL;
  parser->peek_buffer.lexer.position = 0;
  parser->peek_buffer.data = NULL;
  parser->peek_buffer.length = 0;
  parser->peek_buffer.capacity = 0;
  diagnostic_bag_free(&parser->diagnostics);
}
