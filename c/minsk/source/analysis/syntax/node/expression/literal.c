#include "minsk/analysis/syntax/node/expression/literal.h"
#include <malloc.h>

expression_syntax_t* literal_expression_syntax_new(
    syntax_token_t literal_token, object_t* value) {
  literal_expression_syntax_t* syntax =
      malloc(sizeof(literal_expression_syntax_t));
  syntax->base.base.kind = syntax_kind_literal_expression;
  syntax->base.base.is_token = false;
  syntax->literal_token = literal_token;
  if (value == NULL) {
    syntax->value = object_copy(syntax->literal_token.value);
  } else {
    syntax->value = value;
  }
  return (expression_syntax_t*)syntax;
}
void literal_expression_syntax_free(literal_expression_syntax_t* syntax) {
  token_free(&syntax->literal_token);
  object_free(syntax->value);
  free(syntax);
}
syntax_node_children_t literal_expression_syntax_children(
    const literal_expression_syntax_t* syntax) {
  syntax_node_children_t children = {
      .length = 1,
      .data = malloc(sizeof(const syntax_node_t*) * 1),
  };
  children.data[0] = (const syntax_node_t*)&syntax->literal_token;
  return children;
}
