#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/compilation.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/text/source.h"
#include "minsk/analysis/text/span.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"
#include "sds.h"
#include "styler/styler.h"
#include "util/line.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  char* input_line = NULL;
  size_t line_len = 0;
  bool show_tree = false;
  variable_map_t variables;
  variable_map_init(&variables);
  sds text_builder = sdsempty();
  compilation_t* previous = NULL;
  while (true) {
    styler_apply_fg(styler_fg_green, stdout);
    printf("%s", sdslen(text_builder) == 0 ? "» " : "· ");
    styler_apply_fg(styler_fg_reset, stdout);
    if (!util_read_line(&input_line, &line_len, stdin)) {
      break;
    }

    if (strcmp(input_line, "#showTree") == 0) {
      show_tree = !show_tree;
      printf("%s\n",
          show_tree ? "Showing parse trees." : "Not showing parse trees.");
      continue;
    } else if (strcmp(input_line, "#cls") == 0) {
      printf("\x1b[2J\x1b[0;0H");
      continue;
    }

    bool is_empty = input_line[0] == '\0';
    text_builder = sdscatfmt(text_builder, "%s\n", input_line);
    syntax_tree_t syntax_tree = syntax_tree_parse(text_builder);
    if (syntax_tree.diagnostics.length > 0 && !is_empty) {
      syntax_tree_free(&syntax_tree);
      continue;
    }
    if (show_tree) {
      syntax_node_pretty_print((syntax_node_t*)syntax_tree.root.root, stdout);
    }
    compilation_t* compilation;
    if (previous == NULL) {
      compilation = compilation_new(NULL, &syntax_tree);
    } else {
      compilation = compilation_continue_with(previous, &syntax_tree);
    }
    evaluation_result_t result = compilation_evaluate(compilation, &variables);
    diagnostic_bag_t diagnostics = result.diagnostics;
    if (diagnostics.length > 0) {
      compilation_free(compilation);
      for (size_t i = 0; i < diagnostics.length; i++) {
        size_t line_index = source_text_get_line_index(
            &syntax_tree.source_text, diagnostics.data[i].span.start);
        size_t line_number = line_index + 1;
        text_line_t line = syntax_tree.source_text.lines.data[line_index];
        size_t character = diagnostics.data[i].span.start - line.start + 1;
        styler_apply_fg(styler_fg_red, stdout);
        printf("(%zu, %zu): %s\n", line_number, character,
            diagnostics.data[i].message);
        styler_apply_fg(styler_fg_reset, stdout);
        text_span_t prefix_span =
            text_span_from_bounds(line.start, diagnostics.data[i].span.start);
        text_span_t error_span = diagnostics.data[i].span;
        text_span_t suffix_span = text_span_from_bounds(
            text_span_end(diagnostics.data[i].span), text_line_end(line));
        printf("   ");
        printf(
            "%.*s", (int)prefix_span.length, text_builder + prefix_span.start);
        styler_apply_fg(styler_fg_red, stdout);
        printf("%.*s", (int)error_span.length, text_builder + error_span.start);
        styler_apply_fg(styler_fg_reset, stdout);
        printf(
            "%.*s", (int)suffix_span.length, text_builder + suffix_span.start);
        printf("\n");
      }
      diagnostic_bag_free(&diagnostics);
    } else {
      previous = compilation;
      styler_apply_fg(styler_fg_magenta, stdout);
      object_print(result.value, stdout);
      styler_apply_fg(styler_fg_reset, stdout);
      printf("\n");
      object_free(result.value);
    }
    syntax_tree_free(&syntax_tree);
    sdsfree(text_builder);
    text_builder = sdsempty();
  }
  variable_map_free(&variables);
  free(input_line);
  sdsfree(text_builder);
  if (previous != NULL) {
    compilation_free(previous);
  }
  return 0;
}
