#include "minsk_test/analysis/syntax/lexer.h"
#include "check.h"
#include "minsk/analysis/syntax/facts.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/syntax/tokens.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk_test/analysis/syntax/kind_set.h"
#include "minsk_test/analysis/syntax/kind_vector.h"
#include "minsk_test/esc.h"
#include "sds.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  syntax_kind_t kind;
  const char* text;
} simple_token_t;

typedef struct {
  simple_token_t* data;
  size_t length;
  size_t capacity;
} simple_token_vector_t;

static simple_token_vector_t get_tokens(void) {
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
  memcpy(
      tokens.data, dynamic_tokens, sizeof(simple_token_t) * num_dynamic_tokens);

  for (int i = 0; i < syntax_kind_count; i++) {
    const char* text = facts_get_text(i);
    if (text != NULL) {
      simple_token_t token = {
          .kind = i,
          .text = text,
      };
      if (tokens.length == tokens.capacity) {
        tokens.capacity *= 2;
        simple_token_t* new_data =
            realloc(tokens.data, sizeof(simple_token_t) * tokens.capacity);
        assert(new_data != NULL);
        tokens.data = new_data;
      }
      tokens.data[tokens.length] = token;
      tokens.length += 1;
    }
  }

  return tokens;
}

static simple_token_vector_t get_separators(void) {
  static const simple_token_t separators[] = {
      {.kind = syntax_kind_whitespace_token, .text = " "},
      {.kind = syntax_kind_whitespace_token, .text = "  "},
      {.kind = syntax_kind_whitespace_token, .text = "\r"},
      {.kind = syntax_kind_whitespace_token, .text = "\n"},
      {.kind = syntax_kind_whitespace_token, .text = "\r\n"},
  };
  static const size_t num_separators =
      sizeof(separators) / sizeof(simple_token_t);

  simple_token_vector_t all_separators = {
      .data = malloc(sizeof(simple_token_t) * num_separators),
      .length = num_separators,
      .capacity = num_separators,
  };
  memcpy(
      all_separators.data, separators, sizeof(simple_token_t) * num_separators);
  return all_separators;
}

START_TEST(tests_all_tokens_test) {
  syntax_kind_set_t tested_kinds;
  syntax_kind_set_init(&tested_kinds);
  simple_token_vector_t tokens = get_tokens();
  simple_token_vector_t separators = get_separators();
  for (size_t i = 0; i < tokens.length; i++) {
    syntax_kind_set_insert(&tested_kinds, tokens.data[i].kind);
  }
  for (size_t i = 0; i < separators.length; i++) {
    syntax_kind_set_insert(&tested_kinds, separators.data[i].kind);
  }

  for (int i = 0; i < syntax_kind_count; i++) {
    const char* str = syntax_kind_to_string(i);
    if (strstr(str, "token") == NULL && strstr(str, "keyword") == NULL) {
      continue;
    }
    if (i != syntax_kind_bad_token && i != syntax_kind_end_of_file_token &&
        !syntax_kind_set_contains(&tested_kinds, i)) {
      ck_abort_msg("%s is not tested", syntax_kind_to_string(i));
      break;
    }
  }

  syntax_kind_set_free(&tested_kinds);
  free(tokens.data);
  free(separators.data);
}
END_TEST

START_TEST(lexes_token_test) {
  simple_token_vector_t tokens = get_tokens();

  for (size_t i = 0; i < tokens.length; i++) {
    printf("Lexer: tokens[%zu]: '%s'\n", i, tokens.data[i].text);
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

static bool requires_separator(simple_token_t* t1, simple_token_t* t2) {
  bool t1_is_keyword =
      strstr(syntax_kind_to_string(t1->kind), "keyword") != NULL;
  bool t2_is_keyword =
      strstr(syntax_kind_to_string(t2->kind), "keyword") != NULL;

  if ((t1_is_keyword || t1->kind == syntax_kind_identifier_token) &&
      (t2_is_keyword || t2->kind == syntax_kind_identifier_token)) {
    return true;
  }

  if (t1->kind == syntax_kind_number_token &&
      t2->kind == syntax_kind_number_token) {
    return true;
  }

  if ((t1->kind == syntax_kind_bang_token ||
          t1->kind == syntax_kind_equals_token) &&
      (t2->kind == syntax_kind_equals_equals_token ||
          t2->kind == syntax_kind_equals_token)) {
    return true;
  }

  return false;
}

START_TEST(lexes_token_pairs_test) {
  simple_token_vector_t tokens = get_tokens();

  for (size_t i = 0; i < tokens.length; i++) {
    for (size_t j = 0; j < tokens.length; j++) {
      if (requires_separator(&tokens.data[i], &tokens.data[j])) {
        continue;
      }

      sds text = sdscatfmt(
          sdsempty(), "%s%s", tokens.data[i].text, tokens.data[j].text);
      printf("Lexer: token pairs[%zu, %zu]: '%s'\n", i, j, text);
      syntax_token_vector_t lexed_tokens = syntax_tree_parse_tokens(text);
      sdsfree(text);
      ck_assert_int_eq(lexed_tokens.length, 2);
      ck_assert_int_eq(lexed_tokens.data[0].base.kind, tokens.data[i].kind);
      ck_assert_str_eq(lexed_tokens.data[0].text, tokens.data[i].text);
      ck_assert_int_eq(lexed_tokens.data[1].base.kind, tokens.data[j].kind);
      ck_assert_str_eq(lexed_tokens.data[1].text, tokens.data[j].text);
      token_free(&lexed_tokens.data[0]);
      token_free(&lexed_tokens.data[1]);
      syntax_token_vector_free(&lexed_tokens);
    }
  }

  free(tokens.data);
}
END_TEST

START_TEST(lexes_token_pairs_with_separator_test) {
  simple_token_vector_t tokens = get_tokens();
  simple_token_vector_t separators = get_separators();

  for (size_t i = 0; i < tokens.length; i++) {
    for (size_t j = 0; j < tokens.length; j++) {
      if (requires_separator(&tokens.data[i], &tokens.data[j])) {
        for (size_t k = 0; k < separators.length; k++) {
          sds text = sdscatfmt(sdsempty(), "%s%s%s", tokens.data[i].text,
              separators.data[k].text, tokens.data[j].text);
          sds text_escaped = esc(text);
          printf("Lexer: token pairs with separator[%zu, %zu, %zu]: '%s'\n", i,
              k, j, text_escaped);
          sdsfree(text_escaped);
          syntax_token_vector_t lexed_tokens = syntax_tree_parse_tokens(text);
          sdsfree(text);
          ck_assert_int_eq(lexed_tokens.length, 3);
          ck_assert_int_eq(lexed_tokens.data[0].base.kind, tokens.data[i].kind);
          ck_assert_str_eq(lexed_tokens.data[0].text, tokens.data[i].text);
          ck_assert_int_eq(
              lexed_tokens.data[1].base.kind, separators.data[k].kind);
          ck_assert_str_eq(lexed_tokens.data[1].text, separators.data[k].text);
          ck_assert_int_eq(lexed_tokens.data[2].base.kind, tokens.data[j].kind);
          ck_assert_str_eq(lexed_tokens.data[2].text, tokens.data[j].text);
          token_free(&lexed_tokens.data[0]);
          token_free(&lexed_tokens.data[1]);
          token_free(&lexed_tokens.data[2]);
          syntax_token_vector_free(&lexed_tokens);
        }
      }
    }
  }

  free(tokens.data);
  free(separators.data);
}
END_TEST

Suite* lexer_suite(void) {
  Suite* s = suite_create("Lexer");

  TCase* tc_tests_all_tokens = tcase_create("tests all tokens");
  tcase_add_test(tc_tests_all_tokens, tests_all_tokens_test);
  suite_add_tcase(s, tc_tests_all_tokens);

  TCase* tc_lexes_token = tcase_create("lexes token");
  tcase_add_test(tc_lexes_token, lexes_token_test);
  suite_add_tcase(s, tc_lexes_token);

  TCase* tc_lexes_token_pairs = tcase_create("lexes token pairs");
  tcase_add_test(tc_lexes_token_pairs, lexes_token_pairs_test);
  suite_add_tcase(s, tc_lexes_token_pairs);

  TCase* tc_lexes_token_pairs_with_separator =
      tcase_create("lexes token pairs with separator");
  tcase_add_test(tc_lexes_token_pairs_with_separator,
      lexes_token_pairs_with_separator_test);
  suite_add_tcase(s, tc_lexes_token_pairs_with_separator);

  return s;
}
