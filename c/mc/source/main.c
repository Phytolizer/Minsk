#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/syntax/tree.h"
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
    binder_t binder;
    binder_init(&binder);
    bound_expression_t *root =
        binder_bind_expression(&binder, syntax_tree.root);
    diagnostic_bag_t diagnostics;
    diagnostic_bag_init(&diagnostics);
    for (size_t i = 0; i < syntax_tree.diagnostics.length; i++) {
      diagnostic_bag_copy_diagnostic(&diagnostics,
                                     syntax_tree.diagnostics.data[i]);
    }
    for (size_t i = 0; i < binder.diagnostics.length; i++) {
      diagnostic_bag_copy_diagnostic(&diagnostics, binder.diagnostics.data[i]);
    }
    syntax_tree_free(&syntax_tree);
    binder_free(&binder);
    if (diagnostics.length > 0) {
      styler_apply_fg(styler_fg_red, stdout);
      for (size_t i = 0; i < diagnostics.length; i++) {
        printf("%s\n", diagnostics.data[i].message);
      }
      styler_apply_fg(styler_fg_reset, stdout);
      diagnostic_bag_free(&diagnostics);
    } else {
      evaluator_t evaluator;
      evaluator_init(&evaluator, root);
      object_t *result = evaluator_evaluate(&evaluator);
      object_print(result, stdout);
      printf("\n");
      object_free(result);
    }
    bound_node_free((bound_node_t *)root);
  }
  free(line);
  return 0;
}
