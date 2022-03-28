#include "minsk_test/analysis/evaluator.h"
#include "check.h"
#include "minsk/analysis/compilation.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"
#include <stddef.h>
#include <stdio.h>

START_TEST(evaluator_correct_evaluation_test) {
  typedef struct {
    const char *input;
    object_t *expected;
  } evaluator_test_t;

  evaluator_test_t tests[] = {
      {.input = "1", .expected = integer_new(1)},
      {.input = "+1", .expected = integer_new(1)},
      {.input = "-1", .expected = integer_new(-1)},
      {.input = "14 + 12", .expected = integer_new(26)},
      {.input = "12 - 10", .expected = integer_new(2)},
      {.input = "12 * 12", .expected = integer_new(144)},
      {.input = "9 / 3", .expected = integer_new(3)},
      {.input = "(10)", .expected = integer_new(10)},
      {.input = "12 == 3", .expected = boolean_new(false)},
      {.input = "3 == 3", .expected = boolean_new(true)},
      {.input = "12 != 3", .expected = boolean_new(true)},
      {.input = "3 != 3", .expected = boolean_new(false)},
      {.input = "false == false", .expected = boolean_new(true)},
      {.input = "false == true", .expected = boolean_new(false)},
      {.input = "true != false", .expected = boolean_new(true)},
      {.input = "true != true", .expected = boolean_new(false)},
      {.input = "true", .expected = boolean_new(true)},
      {.input = "false", .expected = boolean_new(false)},
      {.input = "!true", .expected = boolean_new(false)},
      {.input = "!false", .expected = boolean_new(true)},
      {.input = "true || false", .expected = boolean_new(true)},
      {.input = "false || false", .expected = boolean_new(false)},
      {.input = "(a = 10) * a", .expected = integer_new(100)},
  };

  for (size_t i = 0; i < sizeof tests / sizeof *tests; i++) {
    printf("Evaluator: positives[%zu]: %s\n", i, tests[i].input);
    syntax_tree_t syntax_tree = syntax_tree_parse(tests[i].input);
    compilation_t compilation;
    compilation_init(&compilation, &syntax_tree);
    variable_map_t variables;
    variable_map_init(&variables);
    evaluation_result_t result = compilation_evaluate(&compilation, &variables);
    compilation_free(&compilation);
    for (size_t i = 0; i < result.diagnostics.length; i++) {
      diagnostic_t diagnostic = result.diagnostics.data[i];
      printf("%s\n", diagnostic.message);
    }
    ck_assert_int_eq(result.diagnostics.length, 0);
    ck_assert(object_equals(result.value, tests[i].expected));
    object_free(result.value);
    variable_map_free(&variables);
    syntax_tree_free(&syntax_tree);
    object_free(tests[i].expected);
  }
}
END_TEST

Suite *evaluator_suite(void) {
  Suite *s = suite_create("Evaluator");

  TCase *tc_evaluator = tcase_create("correct values");
  tcase_add_test(tc_evaluator, evaluator_correct_evaluation_test);
  suite_add_tcase(s, tc_evaluator);

  return s;
}
