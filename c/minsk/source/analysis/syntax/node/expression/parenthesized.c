#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include <malloc.h>
expression_syntax_t* parenthesized_expression_syntax_new(
    syntax_token_t open_parenthesis_token, expression_syntax_t* expression,
    syntax_token_t close_parenthesis_token) {
  parenthesized_expression_syntax_t* syntax =
      malloc(sizeof(parenthesized_expression_syntax_t));
  syntax->base.base.kind = syntax_kind_parenthesized_expression;
  syntax->base.base.is_token = false;
  syntax->open_parenthesis_token = open_parenthesis_token;
  syntax->expression = expression;
  syntax->close_parenthesis_token = close_parenthesis_token;
  return (expression_syntax_t*)syntax;
}
void parenthesized_expression_syntax_free(
    parenthesized_expression_syntax_t* syntax) {
  token_free(&syntax->open_parenthesis_token);
  syntax_node_free((syntax_node_t*)syntax->expression);
  token_free(&syntax->close_parenthesis_token);
  free(syntax);
}
syntax_node_children_t parenthesized_expression_syntax_children(
    parenthesized_expression_syntax_t* syntax) {
  syntax_node_children_t children = {
      .length = 3,
      .data = malloc(sizeof(const syntax_node_t*) * 3),
  };
  children.data[0] = (const syntax_node_t*)&syntax->open_parenthesis_token;
  children.data[1] = (const syntax_node_t*)syntax->expression;
  children.data[2] = (const syntax_node_t*)&syntax->close_parenthesis_token;
  return children;
}
