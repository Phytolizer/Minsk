#include "doctest.h"
#include "fmt/core.h"
#include "magic_enum.hpp"
#include "minsk/analysis/syntax/facts.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/statements/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk_test/parametrize.hpp"
#include <algorithm>
#include <cstddef>
#include <iterator>
#include <ranges>
#include <stack>
#include <string_view>
#include <type_traits>
#include <vector>

using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;
using minsk::analysis::syntax::syntax_tree;
namespace facts = minsk::analysis::syntax::facts;

class asserting_iterator final {
  using container = std::vector<const syntax_node *>;
  container m_container;
  typename container::const_iterator m_iterator;

  static container flatten(const syntax_node *node) {
    container result;
    auto stack = std::stack<const syntax_node *>{};
    stack.push(node);

    while (!stack.empty()) {
      auto n = stack.top();
      stack.pop();
      result.push_back(n);

      auto children = n->children();
      for (std::size_t i = children.size(); i > 0; --i) {
        stack.push(children[i - 1]);
      }
    }
    return result;
  }

public:
  explicit asserting_iterator(const syntax_node *node)
      : m_container(flatten(node)), m_iterator(std::begin(m_container)) {}

  ~asserting_iterator() { CHECK(m_iterator == std::ranges::end(m_container)); }
  asserting_iterator(const asserting_iterator &) = delete;
  asserting_iterator &operator=(const asserting_iterator &) = delete;
  asserting_iterator(asserting_iterator &&) = delete;
  asserting_iterator &operator=(asserting_iterator &&) = delete;

  void assert_node(syntax_kind kind) {
    REQUIRE(m_iterator != std::ranges::end(m_container));
    CHECK((*m_iterator)->kind() == kind);
    CHECK(dynamic_cast<const syntax_token *>(*m_iterator) == nullptr);
    ++m_iterator;
  }

  void assert_token(syntax_kind kind, std::string_view text) {
    REQUIRE(m_iterator != std::ranges::end(m_container));
    CHECK((*m_iterator)->kind() == kind);
    auto token = dynamic_cast<const syntax_token *>(*m_iterator);
    REQUIRE(token != nullptr);
    CHECK(token->text() == text);
    ++m_iterator;
  }
};

std::vector<syntax_kind> get_binary_operators() {
  auto result = std::vector<syntax_kind>{};
  std::ranges::copy_if(
      magic_enum::enum_values<syntax_kind>(), std::back_inserter(result),
      [](syntax_kind k) { return facts::binary_operator_precedence(k) > 0; });
  return result;
}

std::vector<syntax_kind> get_unary_operators() {
  auto result = std::vector<syntax_kind>{};
  std::ranges::copy_if(
      magic_enum::enum_values<syntax_kind>(), std::back_inserter(result),
      [](syntax_kind k) { return facts::unary_operator_precedence(k) > 0; });
  return result;
}

struct syntax_kind_pair {
  syntax_kind op1;
  syntax_kind op2;

  syntax_kind_pair() = default;
  syntax_kind_pair(syntax_kind op1, syntax_kind op2) : op1(op1), op2(op2) {}
};

std::vector<syntax_kind_pair> get_binary_operator_pairs() {
  auto result = std::vector<syntax_kind_pair>{};
  auto binary_operators = get_binary_operators();
  for (auto op1 : binary_operators) {
    for (auto op2 : binary_operators) {
      result.emplace_back(op1, op2);
    }
  }
  return result;
}

std::vector<syntax_kind_pair> get_unary_operator_pairs() {
  auto result = std::vector<syntax_kind_pair>{};
  for (auto op1 : get_unary_operators()) {
    for (auto op2 : get_binary_operators()) {
      result.emplace_back(op1, op2);
    }
  }
  return result;
}

const minsk::analysis::syntax::expression_syntax *
get_expression(const syntax_tree &tree) {
  auto statement = tree.root()->statement();
  return dynamic_cast<
             const minsk::analysis::syntax::expression_statement_syntax *>(
             statement)
      ->expression();
}

TEST_CASE("binary operators honor precedence") {
  syntax_kind_pair data;
  auto pairs = get_binary_operator_pairs();

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, pairs);

  auto op1_text = *facts::get_text(data.op1);
  auto op2_text = *facts::get_text(data.op2);
  auto op1_precedence = facts::binary_operator_precedence(data.op1);
  auto op2_precedence = facts::binary_operator_precedence(data.op2);

  auto text = fmt::format("a {} b {} c", op1_text, op2_text);
  auto syntax_tree = syntax_tree::parse(std::move(text));
  auto expression = get_expression(syntax_tree);

  if (op1_precedence >= op2_precedence) {
    auto it = asserting_iterator{expression};

    it.assert_node(syntax_kind::binary_expression);
    it.assert_node(syntax_kind::binary_expression);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "a");
    it.assert_token(data.op1, op1_text);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "b");
    it.assert_token(data.op2, op2_text);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "c");
  } else {
    auto it = asserting_iterator{expression};

    it.assert_node(syntax_kind::binary_expression);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "a");
    it.assert_token(data.op1, op1_text);
    it.assert_node(syntax_kind::binary_expression);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "b");
    it.assert_token(data.op2, op2_text);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "c");
  }
}

TEST_CASE("unary operators honor precedence") {
  syntax_kind_pair data;
  auto pairs = get_unary_operator_pairs();

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, pairs);

  auto op1_text = *facts::get_text(data.op1);
  auto op2_text = *facts::get_text(data.op2);
  auto op1_precedence = facts::unary_operator_precedence(data.op1);
  auto op2_precedence = facts::binary_operator_precedence(data.op2);

  auto text = fmt::format("{} a {} b", op1_text, op2_text);
  auto syntax_tree = syntax_tree::parse(std::move(text));
  auto expression = get_expression(syntax_tree);

  if (op1_precedence >= op2_precedence) {
    auto it = asserting_iterator{expression};

    it.assert_node(syntax_kind::binary_expression);
    it.assert_node(syntax_kind::unary_expression);
    it.assert_token(data.op1, op1_text);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "a");
    it.assert_token(data.op2, op2_text);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "b");
  } else {
    auto it = asserting_iterator{expression};

    it.assert_node(syntax_kind::unary_expression);
    it.assert_token(data.op1, op1_text);
    it.assert_node(syntax_kind::binary_expression);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "a");
    it.assert_token(data.op2, op2_text);
    it.assert_node(syntax_kind::name_expression);
    it.assert_token(syntax_kind::identifier_token, "b");
  }
}
