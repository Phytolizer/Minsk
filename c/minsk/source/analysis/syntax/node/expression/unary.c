#include "minsk/analysis/syntax/node/expression/unary.h"
#include <malloc.h>
expression_syntax_t *unary_expression_syntax_new(syntax_token_t operator_token,
                                                 expression_syntax_t *operand) {
  unary_expression_syntax_t *syntax = malloc(sizeof(unary_expression_syntax_t));
  syntax->base.base.kind = syntax_kind_unary_expression;
  syntax->base.base.is_token = false;
  syntax->operator_token = operator_token;
  syntax->operand = operand;
  return (expression_syntax_t *)syntax;
}
void unary_expression_syntax_free(unary_expression_syntax_t *syntax) {
  token_free(&syntax->operator_token);
  syntax_node_free((syntax_node_t *)syntax->operand);
  free(syntax);
}
syntax_node_children_t
unary_expression_syntax_children(unary_expression_syntax_t *syntax) {
  syntax_node_children_t children = {
      .length = 2,
      .data = malloc(sizeof(const syntax_node_t *) * 2),
  };
  children.data[0] = (syntax_node_t *)&syntax->operator_token;
  children.data[1] = (syntax_node_t *)syntax->operand;
  return children;
}
