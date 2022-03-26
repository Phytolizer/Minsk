#include "minsk/analysis/syntax/node/expression/assignment.h"
#include "minsk/analysis/syntax/node.h"
#include <stdlib.h>

expression_syntax_t *
assignment_expression_syntax_new(syntax_token_t identifier_token,
                                 syntax_token_t equals_token,
                                 expression_syntax_t *expression) {
  assignment_expression_syntax_t *syntax =
      malloc(sizeof(assignment_expression_syntax_t));
  syntax->base.base.is_token = false;
  syntax->base.base.kind = syntax_kind_assignment_expression;
  syntax->identifier_token = identifier_token;
  syntax->equals_token = equals_token;
  syntax->expression = expression;
  return (expression_syntax_t *)syntax;
}

void assignment_expression_syntax_free(assignment_expression_syntax_t *syntax) {
  token_free(&syntax->identifier_token);
  token_free(&syntax->equals_token);
  syntax_node_free((syntax_node_t *)syntax->expression);
  free(syntax);
}

syntax_node_children_t assignment_expression_syntax_children(
    const assignment_expression_syntax_t *syntax) {
  syntax_node_children_t children = {
      .length = 3,
      .data = malloc(sizeof(const syntax_node_t *) * 3),
  };
  children.data[0] = (const syntax_node_t *)&syntax->identifier_token;
  children.data[1] = (const syntax_node_t *)&syntax->equals_token;
  children.data[2] = (const syntax_node_t *)syntax->expression;
  return children;
}
