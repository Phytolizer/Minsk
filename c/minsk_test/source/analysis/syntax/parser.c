#include "minsk_test/analysis/syntax/parser.h"
#include "check.h"
#include "minsk/analysis/syntax/facts.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk_test/analysis/syntax/asserting_iterator.h"
#include "minsk_test/analysis/syntax/kind_vector.h"
#include "sds.h"
#include <stddef.h>
#include <stdio.h>

static syntax_kind_vector_t get_binary_operators(void) {
  // A binary operator is any for which facts_binary_operator_precedence(k)
  // returns nonzero.
  syntax_kind_vector_t result;
  syntax_kind_vector_init(&result);
  for (int i = 0; i < syntax_kind_count; i++) {
    if (facts_binary_operator_precedence(i) > 0) {
      syntax_kind_vector_push(&result, i);
    }
  }
  return result;
}

static syntax_kind_vector_t get_unary_operators(void) {
  // An unary operator is any for which facts_unary_operator_precedence(k)
  // returns nonzero.
  syntax_kind_vector_t result;
  syntax_kind_vector_init(&result);
  for (int i = 0; i < syntax_kind_count; i++) {
    if (facts_unary_operator_precedence(i) > 0) {
      syntax_kind_vector_push(&result, i);
    }
  }
  return result;
}

START_TEST(parser_binary_operator_precedence_test) {
  // Test binary operator precedence interaction between all pairs of binary
  // operators.
  syntax_kind_vector_t binary_operators = get_binary_operators();

  for (size_t i = 0; i < binary_operators.length; i++) {
    syntax_kind_t left = binary_operators.data[i];
    for (size_t j = 0; j < binary_operators.length; j++) {
      syntax_kind_t right = binary_operators.data[j];
      // Parse the tree, and use the asserting_iterator_t to walk the tree.
      sds text = sdscatfmt(sdsempty(), "a %s b %s c", facts_get_text(left),
                           facts_get_text(right));
      printf("Parser: binary pairs[%zu, %zu]: '%s'\n", i, j, text);
      syntax_tree_t syntax_tree = syntax_tree_parse(text);
      sdsfree(text);

      // Walk the tree, and check that the precedence of each node is correct.
      asserting_iterator_t iterator;
      asserting_iterator_init(&iterator,
                              (const syntax_node_t *)syntax_tree.root);
      if (facts_binary_operator_precedence(left) >=
          facts_binary_operator_precedence(right)) {
        // The expression is binary.
        asserting_iterator_assert_node(&iterator,
                                       syntax_kind_binary_expression);
        // The first subexpression is also binary.
        asserting_iterator_assert_node(&iterator,
                                       syntax_kind_binary_expression);
        // The first nested subexpression is 'a'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "a");
        // Now the left operator appears.
        asserting_iterator_assert_token(&iterator, left, facts_get_text(left));
        // The second nested subexpression is 'b'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "b");
        // Now the right operator appears.
        asserting_iterator_assert_token(&iterator, right,
                                        facts_get_text(right));
        // The third nested subexpression is 'c'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "c");
        // The expression ends here.
      } else {
        // The precedence of the second operator is higher.
        // The first subexpression is binary.
        asserting_iterator_assert_node(&iterator,
                                       syntax_kind_binary_expression);
        // The first nested subexpression is 'a'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "a");
        // Now the left operator appears.
        asserting_iterator_assert_token(&iterator, left, facts_get_text(left));
        // The second nested subexpression is binary.
        asserting_iterator_assert_node(&iterator,
                                       syntax_kind_binary_expression);
        // The first nested subexpression is 'b'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "b");
        // Now the right operator appears.
        asserting_iterator_assert_token(&iterator, right,
                                        facts_get_text(right));
        // The third nested subexpression is 'c'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "c");
        // The expression ends here.
      }
      asserting_iterator_free(&iterator);

      syntax_tree_free(&syntax_tree);
    }
  }

  syntax_kind_vector_free(&binary_operators);
}
END_TEST

START_TEST(parser_unary_operator_precedence_test) {
  // Test unary operators and their interaction with binary ones.
  syntax_kind_vector_t unary_operators = get_unary_operators();
  syntax_kind_vector_t binary_operators = get_binary_operators();

  for (size_t i = 0; i < unary_operators.length; i++) {
    syntax_kind_t unary = unary_operators.data[i];
    for (size_t j = 0; j < binary_operators.length; j++) {
      syntax_kind_t binary = binary_operators.data[j];
      // Parse the tree, and use the asserting_iterator_t to walk the tree.
      sds text = sdscatfmt(sdsempty(), "%s a %s b", facts_get_text(unary),
                           facts_get_text(binary));
      printf("Parser: unary/binary pairs[%zu, %zu]: '%s'\n", i, j, text);
      syntax_tree_t syntax_tree = syntax_tree_parse(text);
      sdsfree(text);

      // Walk the tree, and check that the precedence of each node is correct.
      asserting_iterator_t iterator;
      asserting_iterator_init(&iterator,
                              (const syntax_node_t *)syntax_tree.root);
      if (facts_unary_operator_precedence(unary) >=
          facts_binary_operator_precedence(binary)) {
        // The outermost expression is binary.
        asserting_iterator_assert_node(&iterator,
                                       syntax_kind_binary_expression);
        // The first subexpression is unary.
        asserting_iterator_assert_node(&iterator, syntax_kind_unary_expression);
        // Now the unary operator appears.
        asserting_iterator_assert_token(&iterator, unary,
                                        facts_get_text(unary));
        // The first nested subexpression is 'a'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "a");
        // Now the binary operator appears.
        asserting_iterator_assert_token(&iterator, binary,
                                        facts_get_text(binary));
        // The second nested subexpression is 'b'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "b");
        // The expression ends here.
      } else {
        // The precedence of the binary operator is higher.
        // The outermost expression is unary.
        asserting_iterator_assert_node(&iterator, syntax_kind_unary_expression);
        // Now the unary operator appears.
        asserting_iterator_assert_token(&iterator, unary,
                                        facts_get_text(unary));
        // The first nested subexpression is binary.
        asserting_iterator_assert_node(&iterator,
                                       syntax_kind_binary_expression);
        // The first nested subexpression is 'a'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "a");
        // Now the binary operator appears.
        asserting_iterator_assert_token(&iterator, binary,
                                        facts_get_text(binary));
        // The second nested subexpression is 'b'.
        asserting_iterator_assert_node(&iterator, syntax_kind_name_expression);
        asserting_iterator_assert_token(&iterator, syntax_kind_identifier_token,
                                        "b");
        // The expression ends here.
      }

      asserting_iterator_free(&iterator);

      syntax_tree_free(&syntax_tree);
    }
  }

  syntax_kind_vector_free(&unary_operators);
  syntax_kind_vector_free(&binary_operators);
}
END_TEST

Suite *parser_suite(void) {
  Suite *s = suite_create("Parser");

  TCase *tc_binary_operator_precedence =
      tcase_create("binary operator honors precedence");
  tcase_add_test(tc_binary_operator_precedence,
                 parser_binary_operator_precedence_test);
  suite_add_tcase(s, tc_binary_operator_precedence);

  TCase *tc_unary_operator_precedence =
      tcase_create("unary operator honors precedence");
  tcase_add_test(tc_unary_operator_precedence,
                 parser_unary_operator_precedence_test);
  suite_add_tcase(s, tc_unary_operator_precedence);

  return s;
}
