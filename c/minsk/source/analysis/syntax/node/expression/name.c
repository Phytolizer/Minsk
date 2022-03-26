#include "minsk/analysis/syntax/node/expression/name.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"
#include <stdlib.h>

expression_syntax_t *
name_expression_syntax_new(syntax_token_t identifier_token) {
  name_expression_syntax_t *syntax = malloc(sizeof(name_expression_syntax_t));
  syntax->base.base.is_token = false;
  syntax->base.base.kind = syntax_kind_name_expression;
  syntax->identifier_token = identifier_token;
  return (expression_syntax_t *)syntax;
}

void name_expression_syntax_free(name_expression_syntax_t *syntax) {
  token_free(&syntax->identifier_token);
  free(syntax);
}

syntax_node_children_t
name_expression_syntax_children(const name_expression_syntax_t *syntax) {
  syntax_node_children_t children = {
      .length = 1,
      .data = malloc(sizeof(const syntax_node_t *) * 1),
  };
  children.data[0] = (const syntax_node_t *)&syntax->identifier_token;
  return children;
}
