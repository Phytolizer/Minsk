#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/compilation.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/text/span.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"
#include "styler/styler.h"
#include "util/line.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  char *line = NULL;
  size_t line_len = 0;
  bool show_tree = false;
  variable_map_t variables;
  variable_map_init(&variables);
  while (true) {
    printf("> ");
    if (!util_read_line(&line, &line_len, stdin)) {
      break;
    }

    if (strcmp(line, "#showTree") == 0) {
      show_tree = !show_tree;
      printf("%s\n",
             show_tree ? "Showing parse trees." : "Not showing parse trees.");
      continue;
    } else if (strcmp(line, "#cls") == 0) {
      printf("\x1b[2J\x1b[0;0H");
      continue;
    }

    syntax_tree_t syntax_tree = syntax_tree_parse(line);
    if (show_tree) {
      syntax_node_pretty_print((syntax_node_t *)syntax_tree.root, stdout);
    }
    compilation_t compilation;
    compilation_init(&compilation, &syntax_tree);
    evaluation_result_t result = compilation_evaluate(&compilation, &variables);
    syntax_tree_free(&syntax_tree);
    diagnostic_bag_t diagnostics = result.diagnostics;
    if (diagnostics.length > 0) {
      for (size_t i = 0; i < diagnostics.length; i++) {
        styler_apply_fg(styler_fg_red, stdout);
        printf("%s\n", diagnostics.data[i].message);
        styler_apply_fg(styler_fg_reset, stdout);
        text_span_t prefix_span = {.start = 0,
                                   .length = diagnostics.data[i].span.start};
        text_span_t error_span = diagnostics.data[i].span;
        text_span_t suffix_span = {
            .start = text_span_end(diagnostics.data[i].span),
            .length = strlen(line) - text_span_end(diagnostics.data[i].span)};
        printf("   ");
        printf("%.*s", (int)prefix_span.length, line + prefix_span.start);
        styler_apply_fg(styler_fg_red, stdout);
        printf("%.*s", (int)error_span.length, line + error_span.start);
        styler_apply_fg(styler_fg_reset, stdout);
        printf("%.*s", (int)suffix_span.length, line + suffix_span.start);
        printf("\n");
      }
      diagnostic_bag_free(&diagnostics);
    } else {
      object_print(result.value, stdout);
      printf("\n");
      object_free(result.value);
    }
  }
  variable_map_free(&variables);
  free(line);
  return 0;
}
