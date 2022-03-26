#include "minsk_test/analysis/syntax/lexer.h"
#include "check.h"
#include <stdbool.h>
START_TEST(lexes_token_test) { ck_assert(false); }
END_TEST

Suite *lexer_suite(void) {
  Suite *s = suite_create("Lexer");

  TCase *tc_lexes_token = tcase_create("lexes token");
  tcase_add_test(tc_lexes_token, lexes_token_test);
  suite_add_tcase(s, tc_lexes_token);

  return s;
}
