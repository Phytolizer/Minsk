#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/parser.h"
#include "minsk/analysis/syntax/token.h"

void syntax_tree_free(syntax_tree_t *tree) {
  syntax_node_free((syntax_node_t *)tree->root);
  tree->root = NULL;
  token_free(&tree->end_of_file_token);
  diagnostic_bag_free(&tree->diagnostics);
}

syntax_tree_t syntax_tree_parse(const char *text) {
  parser_t parser;
  parser_init(&parser, text);
  syntax_tree_t tree = parser_parse(&parser);
  parser_free(&parser);
  return tree;
}
