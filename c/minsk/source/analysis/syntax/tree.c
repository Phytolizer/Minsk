#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/lexer.h"
#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/parser.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/syntax/tokens.h"
#include "minsk/analysis/text/source.h"

void syntax_tree_free(syntax_tree_t *tree) {
  syntax_node_free((syntax_node_t *)tree->root);
  tree->root = NULL;
  token_free(&tree->end_of_file_token);
  diagnostic_bag_free(&tree->diagnostics);
  source_text_free(&tree->source_text);
}

syntax_tree_t syntax_tree_parse(const char *text) {
  parser_t parser;
  parser_init(&parser, source_text_from(sdsnew(text)));
  syntax_tree_t tree = parser_parse(&parser);
  parser_free(&parser);
  return tree;
}

syntax_token_vector_t syntax_tree_parse_tokens(const char *text) {
  lexer_t lexer;
  lexer_init(&lexer, source_text_from(sdsnew(text)));
  syntax_token_vector_t tokens;
  syntax_token_vector_init(&tokens);
  while (true) {
    syntax_token_t token = lexer_next_token(&lexer);
    if (token.base.kind == syntax_kind_end_of_file_token) {
      token_free(&token);
      break;
    }
    syntax_token_vector_push(&tokens, token);
  }
  source_text_free(&lexer.text);
  lexer_free(&lexer);

  return tokens;
}
