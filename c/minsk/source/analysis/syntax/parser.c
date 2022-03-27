#include "minsk/analysis/syntax/parser.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/facts.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/expression/assignment.h"
#include "minsk/analysis/syntax/node/expression/binary.h"
#include "minsk/analysis/syntax/node/expression/literal.h"
#include "minsk/analysis/syntax/node/expression/name.h"
#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include "minsk/analysis/syntax/node/expression/unary.h"
#include "minsk/analysis/syntax/peek_buffer.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/text/source.h"
#include "minsk/runtime/object.h"
#include <stdbool.h>
#include <stddef.h>

static syntax_token_t *current(parser_t *parser) {
  return peek_buffer_peek(&parser->peek_buffer, 0);
}

static syntax_token_t match_token(parser_t *parser, syntax_kind_t kind) {
  if (current(parser)->base.kind == kind) {
    return peek_buffer_pop(&parser->peek_buffer);
  }

  diagnostic_bag_report_unexpected_token(&parser->diagnostics,
                                         token_span(current(parser)),
                                         current(parser)->base.kind, kind);
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

static expression_syntax_t *parse_boolean_literal(parser_t *parser) {
  bool is_true = current(parser)->base.kind == syntax_kind_true_keyword;
  syntax_token_t token = match_token(
      parser, is_true ? syntax_kind_true_keyword : syntax_kind_false_keyword);
  return literal_expression_syntax_new(token, boolean_new(is_true));
}

static expression_syntax_t *parse_number_literal(parser_t *parser) {
  syntax_token_t number_token = match_token(parser, syntax_kind_number_token);
  return literal_expression_syntax_new(number_token, NULL);
}

static expression_syntax_t *parse_name_expression(parser_t *parser) {
  syntax_token_t identifier_token =
      match_token(parser, syntax_kind_identifier_token);
  return name_expression_syntax_new(identifier_token);
}

static expression_syntax_t *parse_primary_expression(parser_t *parser) {
  switch (current(parser)->base.kind) {
  case syntax_kind_open_parenthesis_token:
    return parse_parenthesized_expression(parser);
  case syntax_kind_true_keyword:
  case syntax_kind_false_keyword:
    return parse_boolean_literal(parser);
  case syntax_kind_number_token:
    return parse_number_literal(parser);
  default:
    return parse_name_expression(parser);
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

static expression_syntax_t *parse_assignment_expression(parser_t *parser) {
  if (current(parser)->base.kind == syntax_kind_identifier_token &&
      peek_buffer_peek(&parser->peek_buffer, 1)->base.kind ==
          syntax_kind_equals_token) {
    syntax_token_t identifier_token =
        match_token(parser, syntax_kind_identifier_token);
    syntax_token_t equals_token = match_token(parser, syntax_kind_equals_token);
    expression_syntax_t *expression = parse_expression(parser);
    return assignment_expression_syntax_new(identifier_token, equals_token,
                                            expression);
  }

  return parse_binary_expression(parser, 0);
}

static expression_syntax_t *parse_expression(parser_t *parser) {
  return parse_assignment_expression(parser);
}

void parser_init(parser_t *parser, source_text_t text) {
  peek_buffer_init(&parser->peek_buffer, text);
  diagnostic_bag_init(&parser->diagnostics);
}
syntax_tree_t parser_parse(parser_t *parser) {
  expression_syntax_t *expression = parse_expression(parser);
  syntax_token_t end_of_file_token =
      match_token(parser, syntax_kind_end_of_file_token);
  diagnostic_bag_t bag;
  diagnostic_bag_init(&bag);
  for (size_t i = 0; i < parser->peek_buffer.lexer.diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(
        &bag, parser->peek_buffer.lexer.diagnostics.data[i]);
  }
  for (size_t i = 0; i < parser->diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(&bag, parser->diagnostics.data[i]);
  }
  return (syntax_tree_t){
      .root = expression,
      .end_of_file_token = end_of_file_token,
      .diagnostics = bag,
      .source_text = parser->peek_buffer.lexer.text,
  };
}
void parser_free(parser_t *parser) {
  peek_buffer_free(&parser->peek_buffer);
  parser->peek_buffer.lexer.text = (source_text_t){0};
  parser->peek_buffer.lexer.position = 0;
  parser->peek_buffer.data = NULL;
  parser->peek_buffer.length = 0;
  parser->peek_buffer.capacity = 0;
  diagnostic_bag_free(&parser->diagnostics);
}
