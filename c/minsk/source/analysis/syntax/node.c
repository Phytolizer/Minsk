#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/node/expression/binary.h"
#include "minsk/analysis/syntax/node/expression/literal.h"
#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include "minsk/analysis/syntax/node/expression/unary.h"
#include "sds.h"
#include "styler/styler.h"
#include <assert.h>
#include <malloc.h>
#include <stdbool.h>

void syntax_node_free(syntax_node_t *node) {
  switch (node->kind) {
  case syntax_kind_binary_expression:
    binary_expression_syntax_free((binary_expression_syntax_t *)node);
    break;
  case syntax_kind_literal_expression:
    literal_expression_syntax_free((literal_expression_syntax_t *)node);
    break;
  case syntax_kind_parenthesized_expression:
    parenthesized_expression_syntax_free(
        (parenthesized_expression_syntax_t *)node);
    break;
  case syntax_kind_unary_expression:
    unary_expression_syntax_free((unary_expression_syntax_t *)node);
    break;
  default:
    assert(false && "corrupt node kind");
  }
}
syntax_node_children_t syntax_node_children(const syntax_node_t *node) {
  if (node->is_token) {
    return (syntax_node_children_t){0};
  }
  switch (node->kind) {
  case syntax_kind_binary_expression:
    return binary_expression_syntax_children(
        (binary_expression_syntax_t *)node);
  case syntax_kind_literal_expression:
    return literal_expression_syntax_children(
        (literal_expression_syntax_t *)node);
  case syntax_kind_parenthesized_expression:
    return parenthesized_expression_syntax_children(
        (parenthesized_expression_syntax_t *)node);
  case syntax_kind_unary_expression:
    return unary_expression_syntax_children((unary_expression_syntax_t *)node);
  default:
    assert(false && "corrupt syntax node");
    return (syntax_node_children_t){0};
  }
}
static void pretty_print(const syntax_node_t *node, FILE *stream,
                         bool is_to_console, sds indent, bool is_last) {
  fprintf(stream, "%s", indent);
  fprintf(stream, "%s", is_last ? "└── " : "├── ");
  if (is_to_console) {
    styler_apply_fg(node->is_token ? styler_fg_blue : styler_fg_cyan, stream);
  }
  syntax_kind_print(node->kind, stream);
  if (is_to_console) {
    styler_apply_fg(styler_fg_reset, stream);
  }
  if (node->is_token) {
    syntax_token_t *tok = (syntax_token_t *)node;
    if (tok->value != NULL) {
      fprintf(stream, " ");
      object_print(tok->value, stream);
    }
  }
  fprintf(stream, "\n");
  sds new_indent =
      sdscatfmt(sdsempty(), "%S%s", indent, is_last ? "    " : "│   ");
  sdsfree(indent);
  syntax_node_children_t children = syntax_node_children(node);
  if (children.length != 0) {
    for (size_t i = 0; i < children.length; i++) {
      pretty_print(children.data[i], stream, is_to_console, sdsdup(new_indent),
                   i == children.length - 1);
    }
  }
  sdsfree(new_indent);
}
void syntax_node_pretty_print(const syntax_node_t *node, FILE *stream) {
  pretty_print(node, stream, stream == stdout, sdsempty(), true);
}
void syntax_node_children_free(syntax_node_children_t *children) {
  free(children->data);
  children->data = NULL;
  children->length = 0;
}
