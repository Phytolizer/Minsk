#include "minsk_test/analysis/syntax/lexer.h"
#include "check.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/syntax/tokens.h"
#include "minsk/analysis/syntax/tree.h"
#include <stdbool.h>
#include <stdlib.h>

typedef struct {
  syntax_kind_t kind;
  const char *text;
} simple_token_t;

typedef struct {
  simple_token_t *data;
  size_t length;
  size_t capacity;
} simple_token_vector_t;

simple_token_vector_t get_tokens(void) {
  static const simple_token_t dynamic_tokens[] = {
      {.kind = syntax_kind_number_token, .text = "1"},
      {.kind = syntax_kind_number_token, .text = "123"},
      {.kind = syntax_kind_identifier_token, .text = "a"},
      {.kind = syntax_kind_identifier_token, .text = "abc"},
  };
  static const size_t num_dynamic_tokens =
      sizeof(dynamic_tokens) / sizeof(simple_token_t);

  simple_token_vector_t tokens = {
      .data = malloc(sizeof(simple_token_t) * num_dynamic_tokens),
      .length = num_dynamic_tokens,
      .capacity = num_dynamic_tokens,
  };
  memcpy(tokens.data, dynamic_tokens,
         sizeof(simple_token_t) * num_dynamic_tokens);

  return tokens;
}

START_TEST(lexes_token_test) {
  simple_token_vector_t tokens = get_tokens();

  for (size_t i = 0; i < tokens.length; i++) {
    syntax_token_vector_t lexed_tokens =
        syntax_tree_parse_tokens(tokens.data[i].text);
    ck_assert_int_eq(lexed_tokens.length, 1);
    ck_assert_int_eq(lexed_tokens.data[0].base.kind, tokens.data[i].kind);
    ck_assert_str_eq(lexed_tokens.data[0].text, tokens.data[i].text);
    token_free(&lexed_tokens.data[0]);
    syntax_token_vector_free(&lexed_tokens);
  }

  free(tokens.data);
}
END_TEST

Suite *lexer_suite(void) {
  Suite *s = suite_create("Lexer");

  TCase *tc_lexes_token = tcase_create("lexes token");
  tcase_add_test(tc_lexes_token, lexes_token_test);
  suite_add_tcase(s, tc_lexes_token);

  return s;
}
