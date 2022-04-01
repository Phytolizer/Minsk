#include "doctest.h"
#include "fmt/core.h"
#include "magic_enum.hpp"
#include "minsk/analysis/syntax/facts.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk_test/parametrize.hpp"
#include <algorithm>
#include <array>
#include <iterator>
#include <ranges>
#include <string_view>
#include <unordered_set>
#include <vector>

using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_tree;
namespace facts = minsk::analysis::syntax::facts;

struct simple_token final {
  syntax_kind kind;
  std::string text;
};

std::vector separators = {
    simple_token{syntax_kind::whitespace_token, " "},
    simple_token{syntax_kind::whitespace_token, "  "},
    simple_token{syntax_kind::whitespace_token, "\r"},
    simple_token{syntax_kind::whitespace_token, "\n"},
    simple_token{syntax_kind::whitespace_token, "\r\n"},
};

std::array dynamic_tokens = {
    simple_token{syntax_kind::identifier_token, "a"},
    simple_token{syntax_kind::identifier_token, "abc"},
    simple_token{syntax_kind::number_token, "1"},
    simple_token{syntax_kind::number_token, "123"},
};

std::vector<simple_token> get_tokens() {
  auto result = std::vector<simple_token>{};
  std::copy(dynamic_tokens.begin(), dynamic_tokens.end(),
            std::back_inserter(result));

  for (auto kind : magic_enum::enum_values<syntax_kind>()) {
    auto text = facts::get_text(kind);
    if (text) {
      result.emplace_back(simple_token{kind, *text});
    }
  }

  return result;
}

struct simple_token_pair final {
  simple_token t1;
  simple_token t2;
};

bool requires_separator(const simple_token &t1, const simple_token &t2) {
  bool t1_is_keyword = magic_enum::enum_name(t1.kind).ends_with("keyword");
  bool t2_is_keyword = magic_enum::enum_name(t2.kind).ends_with("keyword");

  if ((t1_is_keyword || t1.kind == syntax_kind::identifier_token) &&
      (t2_is_keyword || t2.kind == syntax_kind::identifier_token)) {
    return true;
  }

  if (t1.kind == syntax_kind::number_token &&
      t2.kind == syntax_kind::number_token) {
    return true;
  }

  if ((t1.kind == syntax_kind::bang_token ||
       t1.kind == syntax_kind::equals_token ||
       t1.kind == syntax_kind::less_token ||
       t1.kind == syntax_kind::greater_token) &&
      (t2.kind == syntax_kind::equals_equals_token ||
       t2.kind == syntax_kind::equals_token)) {
    return true;
  }

  if (t1.kind == syntax_kind::ampersand_token &&
      (t2.kind == syntax_kind::ampersand_token ||
       t2.kind == syntax_kind::ampersand_ampersand_token)) {
    return true;
  }

  if (t1.kind == syntax_kind::pipe_token &&
      (t2.kind == syntax_kind::pipe_token ||
       t2.kind == syntax_kind::pipe_pipe_token)) {
    return true;
  }

  return false;
}

std::vector<simple_token_pair> get_token_pairs() {
  auto tokens = get_tokens();
  auto result = std::vector<simple_token_pair>{};
  for (const auto &t1 : tokens) {
    for (const auto &t2 : tokens) {
      if (!requires_separator(t1, t2)) {
        result.emplace_back(simple_token_pair{t1, t2});
      }
    }
  }
  return result;
}

struct simple_token_pair_with_separator final {
  simple_token t1;
  simple_token sep;
  simple_token t2;
};

std::vector<simple_token_pair_with_separator> get_token_pairs_with_separator() {
  auto result = std::vector<simple_token_pair_with_separator>{};
  auto tokens = get_tokens();
  for (const auto &t1 : tokens) {
    for (const auto &t2 : tokens) {
      if (requires_separator(t1, t2)) {
        for (const auto &sep : separators) {
          result.emplace_back(simple_token_pair_with_separator{t1, sep, t2});
        }
      }
    }
  }
  return result;
}

TEST_CASE("tests all tokens") {
  auto token_kinds = std::unordered_set<syntax_kind>{};
  std::ranges::copy_if(
      magic_enum::enum_values<syntax_kind>(),
      std::inserter(token_kinds, token_kinds.end()), [](syntax_kind k) {
        auto name = magic_enum::enum_name(k);
        return name.ends_with("keyword") || name.ends_with("token");
      });

  auto tested_token_kinds = std::unordered_set<syntax_kind>{};
  std::ranges::transform(
      get_tokens(), std::inserter(tested_token_kinds, tested_token_kinds.end()),
      [](const simple_token &tok) { return tok.kind; });
  std::ranges::transform(
      separators, std::inserter(tested_token_kinds, tested_token_kinds.end()),
      [](const simple_token &tok) { return tok.kind; });

  auto untested_token_kinds = token_kinds;
  std::erase_if(untested_token_kinds, [&tested_token_kinds](const auto &k) {
    return k == syntax_kind::bad_token || k == syntax_kind::end_of_file_token ||
           tested_token_kinds.find(k) != tested_token_kinds.end();
  });
  CHECK(untested_token_kinds.size() == 0);
}

TEST_CASE("lexes token") {
  simple_token data;
  auto tokens = get_tokens();

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, tokens);

  auto lexed_tokens = syntax_tree::parse_tokens(std::string{data.text});
  REQUIRE(lexed_tokens.size() == 1);
  CHECK(lexed_tokens[0].kind() == data.kind);
  CHECK(lexed_tokens[0].text() == data.text);
}

TEST_CASE("lexes token pairs") {
  simple_token_pair data;
  auto token_pairs = get_token_pairs();

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, token_pairs);

  auto text = fmt::format("{}{}", data.t1.text, data.t2.text);
  auto tokens = syntax_tree::parse_tokens(std::move(text));
  REQUIRE(tokens.size() == 2);
  CHECK(tokens[0].kind() == data.t1.kind);
  CHECK(tokens[0].text() == data.t1.text);
  CHECK(tokens[1].kind() == data.t2.kind);
  CHECK(tokens[1].text() == data.t2.text);
}

TEST_CASE("lexes token pairs with separator") {
  simple_token_pair_with_separator data;
  auto token_pairs_with_separator = get_token_pairs_with_separator();

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, token_pairs_with_separator);

  auto text = fmt::format("{}{}{}", data.t1.text, data.sep.text, data.t2.text);
  auto tokens = syntax_tree::parse_tokens(std::move(text));
  REQUIRE(tokens.size() == 3);
  CHECK(tokens[0].kind() == data.t1.kind);
  CHECK(tokens[0].text() == data.t1.text);
  CHECK(tokens[1].kind() == data.sep.kind);
  CHECK(tokens[1].text() == data.sep.text);
  CHECK(tokens[2].kind() == data.t2.kind);
  CHECK(tokens[2].text() == data.t2.text);
}
