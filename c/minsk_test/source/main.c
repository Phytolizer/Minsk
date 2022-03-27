#include "minsk_test/analysis/evaluator.h"
#include "minsk_test/analysis/syntax/lexer.h"
#include "minsk_test/analysis/syntax/parser.h"
#include <check.h>
#include <stdlib.h>

int main(void) {
  Suite *lexer_s = lexer_suite();
  SRunner *sr = srunner_create(lexer_s);
  srunner_set_fork_status(sr, CK_NOFORK);
  srunner_run_all(sr, CK_NORMAL);
  int number_failed = srunner_ntests_failed(sr);
  srunner_free(sr);
  if (number_failed > 0) {
    return EXIT_FAILURE;
  }

  Suite *parser_s = parser_suite();
  sr = srunner_create(parser_s);
  srunner_set_fork_status(sr, CK_NOFORK);
  srunner_run_all(sr, CK_NORMAL);
  number_failed += srunner_ntests_failed(sr);
  srunner_free(sr);
  if (number_failed > 0) {
    return EXIT_FAILURE;
  }

  Suite *evaluator_s = evaluator_suite();
  sr = srunner_create(evaluator_s);
  srunner_set_fork_status(sr, CK_NOFORK);
  srunner_run_all(sr, CK_NORMAL);
  number_failed += srunner_ntests_failed(sr);
  srunner_free(sr);
  if (number_failed > 0) {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
