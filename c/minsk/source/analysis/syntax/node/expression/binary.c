#include "minsk/analysis/syntax/node/expression/binary.h"
#include <malloc.h>

expression_syntax_t *binary_expression_syntax_new(expression_syntax_t *left,
                                                  syntax_token_t operator_token,
                                                  expression_syntax_t *right) {
  binary_expression_syntax_t *syntax =
      malloc(sizeof(binary_expression_syntax_t));
  syntax->base.base.kind = syntax_kind_binary_expression;
  syntax->base.base.is_token = false;
  syntax->left = left;
  syntax->operator_token = operator_token;
  syntax->right = right;
  return (expression_syntax_t *)syntax;
}
void binary_expression_syntax_free(binary_expression_syntax_t *syntax) {
  syntax_node_free((syntax_node_t *)syntax->left);
  token_free(&syntax->operator_token);
  syntax_node_free((syntax_node_t *)syntax->right);
  free(syntax);
}
syntax_node_children_t
binary_expression_syntax_children(const binary_expression_syntax_t *syntax) {
  syntax_node_children_t children = {
      .length = 3,
      .data = malloc(sizeof(const syntax_node_t *) * 3),
  };
  children.data[0] = (const syntax_node_t *)syntax->left;
  children.data[1] = (const syntax_node_t *)&syntax->operator_token;
  children.data[2] = (const syntax_node_t *)syntax->right;
  return children;
}
