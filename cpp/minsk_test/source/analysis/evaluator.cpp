#include "doctest.h"
#include "minsk/analysis/compilation.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/analysis/variable_map.hpp"
#include "minsk/runtime/object.hpp"
#include "minsk_test/parametrize.hpp"
#include <array>
#include <string_view>

using minsk::runtime::boolean;
using minsk::runtime::integer;

struct evaluator_test {
  std::string_view text;
  minsk::runtime::object_ptr expected;

  evaluator_test() = default;
  evaluator_test(std::string_view text, minsk::runtime::object_ptr expected)
      : text(text), expected(std::move(expected)) {}
  evaluator_test &operator=(const evaluator_test &other) {
    if (&other != this) {
      text = other.text;
      expected = minsk::runtime::copy_object_ptr(other.expected.get());
    }
    return *this;
  }
};

TEST_CASE("correct evaluation") {
  evaluator_test data;
  auto evaluator_tests = std::array{
      evaluator_test{"1", std::make_unique<integer>(1)},
      evaluator_test{"+1", std::make_unique<integer>(1)},
      evaluator_test{"-1", std::make_unique<integer>(-1)},
      evaluator_test{"14 + 12", std::make_unique<integer>(26)},
      evaluator_test{"12 - 3", std::make_unique<integer>(9)},
      evaluator_test{"4 * 2", std::make_unique<integer>(8)},
      evaluator_test{"9 / 3", std::make_unique<integer>(3)},
      evaluator_test{"(10)", std::make_unique<integer>(10)},
      evaluator_test{"12 == 3", std::make_unique<boolean>(false)},
      evaluator_test{"3 == 3", std::make_unique<boolean>(true)},
      evaluator_test{"12 != 3", std::make_unique<boolean>(true)},
      evaluator_test{"3 != 3", std::make_unique<boolean>(false)},
      evaluator_test{"false == false", std::make_unique<boolean>(true)},
      evaluator_test{"false == true", std::make_unique<boolean>(false)},
      evaluator_test{"true != false", std::make_unique<boolean>(true)},
      evaluator_test{"true != true", std::make_unique<boolean>(false)},
      evaluator_test{"true", std::make_unique<boolean>(true)},
      evaluator_test{"false", std::make_unique<boolean>(false)},
      evaluator_test{"!true", std::make_unique<boolean>(false)},
      evaluator_test{"!false", std::make_unique<boolean>(true)},
      evaluator_test{"true || false", std::make_unique<boolean>(true)},
      evaluator_test{"false || false", std::make_unique<boolean>(false)},
      evaluator_test{"{ var a = 0 (a = 10) * a }",
                     std::make_unique<integer>(100)},
  };

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, evaluator_tests);

  auto syntax_tree =
      minsk::analysis::syntax::syntax_tree::parse(std::string{data.text});
  auto compilation = minsk::analysis::compilation{std::move(syntax_tree)};
  auto variables = minsk::analysis::variable_map{};
  auto result = compilation.evaluate(&variables);
  REQUIRE(result.diagnostics().size() == 0);
  CHECK(*result.value() == *data.expected);
}
