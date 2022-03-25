#include "minsk/analysis/syntax/facts.hpp"
#include "doctest.h"
#include "magic_enum.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk_test/parametrize.hpp"
#include <algorithm>
#include <optional>
#include <ranges>
#include <vector>

using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_tree;
namespace facts = minsk::analysis::syntax::facts;

std::vector<syntax_kind> get_kinds_with_text() {
  std::vector<syntax_kind> result;
  std::ranges::copy_if(magic_enum::enum_values<syntax_kind>(),
                       std::back_inserter(result),
                       [](minsk::analysis::syntax::syntax_kind k) {
                         return facts::get_text(k) != std::nullopt;
                       });
  return result;
}

TEST_CASE("get_text() round trips") {
  syntax_kind data;
  auto kinds = get_kinds_with_text();

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, kinds);

  auto text = facts::get_text(data);
  auto tokens = syntax_tree::parse_tokens(*text);
  REQUIRE(tokens.size() == 1);
  CHECK(tokens[0].kind() == data);
  CHECK(tokens[0].text() == text);
}
