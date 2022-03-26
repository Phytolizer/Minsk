#include <check.h>
#include <stdlib.h>
#include "minsk_test/analysis/syntax/lexer.h"

int main(void) {
  Suite *lexer_s = lexer_suite();
  SRunner *sr = srunner_create(lexer_s);
  srunner_set_fork_status(sr, CK_NOFORK);
  srunner_run_all(sr, CK_NORMAL);
  int number_failed = srunner_ntests_failed(sr);
  srunner_free(sr);
  return number_failed == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
