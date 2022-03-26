#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/lexer.h"
#include "minsk/analysis/syntax/parser.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/runtime/object.h"
#include "sds.h"
#include "util/line.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  char *line = NULL;
  size_t line_len = 0;
  while (true) {
    printf("> ");
    if (!util_read_line(&line, &line_len, stdin)) {
      break;
    }

    parser_t parser;
    parser_init(&parser, line);
    expression_syntax_t *expression = parser_parse(&parser);
    parser_free(&parser);
    syntax_node_pretty_print((syntax_node_t *)expression, stdout);
    evaluator_t evaluator;
    evaluator_init(&evaluator, expression);
    object_t *result = evaluator_evaluate(&evaluator);
    syntax_node_free((syntax_node_t *)expression);
    object_print(result, stdout);
    printf("\n");
    object_free(result);
  }
  free(line);
  return 0;
}
