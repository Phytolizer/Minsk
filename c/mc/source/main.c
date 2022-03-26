#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/syntax/parser.h"
#include "minsk/runtime/object.h"
#include "util/line.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

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
    diagnostic_bag_t diagnostics = parser.diagnostics;
    syntax_node_pretty_print((syntax_node_t *)expression, stdout);
    if (diagnostics.length > 0) {
      for (size_t i = 0; i < diagnostics.length; i++) {
        printf("%s\n", diagnostics.data[i].message);
      }
    } else {
      evaluator_t evaluator;
      evaluator_init(&evaluator, expression);
      object_t *result = evaluator_evaluate(&evaluator);
      object_print(result, stdout);
      printf("\n");
      object_free(result);
    }
    parser_free(&parser);
    syntax_node_free((syntax_node_t *)expression);
  }
  free(line);
  return 0;
}
