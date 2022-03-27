#include "doctest.h"
#include "minsk/analysis/compilation.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/analysis/variable_map.hpp"
#include "minsk/runtime/object.hpp"
#include "minsk_test/analysis/text/annotated.hpp"
#include "minsk_test/parametrize.hpp"
#include <array>
#include <cstddef>
#include <stdexcept>
#include <string_view>

using minsk::runtime::boolean;
using minsk::runtime::integer;
using minsk_test::analysis::text::annotated_text;

struct evaluation_test {
  std::string_view text;
  minsk::runtime::object_ptr expected;

  evaluation_test() = default;
  evaluation_test(std::string_view text, minsk::runtime::object_ptr expected)
      : text(text), expected(std::move(expected)) {}
  evaluation_test &operator=(const evaluation_test &other) {
    if (&other != this) {
      text = other.text;
      expected = minsk::runtime::copy_object_ptr(other.expected.get());
    }
    return *this;
  }
};

TEST_CASE("correct evaluation") {
  evaluation_test data;
  auto evaluator_tests = std::array{
      evaluation_test{"1", std::make_unique<integer>(1)},
      evaluation_test{"+1", std::make_unique<integer>(1)},
      evaluation_test{"-1", std::make_unique<integer>(-1)},
      evaluation_test{"14 + 12", std::make_unique<integer>(26)},
      evaluation_test{"12 - 3", std::make_unique<integer>(9)},
      evaluation_test{"4 * 2", std::make_unique<integer>(8)},
      evaluation_test{"9 / 3", std::make_unique<integer>(3)},
      evaluation_test{"(10)", std::make_unique<integer>(10)},
      evaluation_test{"12 == 3", std::make_unique<boolean>(false)},
      evaluation_test{"3 == 3", std::make_unique<boolean>(true)},
      evaluation_test{"12 != 3", std::make_unique<boolean>(true)},
      evaluation_test{"3 != 3", std::make_unique<boolean>(false)},
      evaluation_test{"3 > 3", std::make_unique<boolean>(false)},
      evaluation_test{"3 >= 3", std::make_unique<boolean>(true)},
      evaluation_test{"3 < 3", std::make_unique<boolean>(false)},
      evaluation_test{"3 <= 3", std::make_unique<boolean>(true)},
      evaluation_test{"4 > 3", std::make_unique<boolean>(true)},
      evaluation_test{"4 >= 3", std::make_unique<boolean>(true)},
      evaluation_test{"4 < 3", std::make_unique<boolean>(false)},
      evaluation_test{"4 <= 3", std::make_unique<boolean>(false)},
      evaluation_test{"false == false", std::make_unique<boolean>(true)},
      evaluation_test{"false == true", std::make_unique<boolean>(false)},
      evaluation_test{"true != false", std::make_unique<boolean>(true)},
      evaluation_test{"true != true", std::make_unique<boolean>(false)},
      evaluation_test{"true", std::make_unique<boolean>(true)},
      evaluation_test{"false", std::make_unique<boolean>(false)},
      evaluation_test{"!true", std::make_unique<boolean>(false)},
      evaluation_test{"!false", std::make_unique<boolean>(true)},
      evaluation_test{"true || false", std::make_unique<boolean>(true)},
      evaluation_test{"false || false", std::make_unique<boolean>(false)},
      evaluation_test{"{ var a = 0 (a = 10) * a }",
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

void assert_has_diagnostics(std::string_view text,
                            std::string_view diagnostic_text) {
  auto annotated = annotated_text::parse(text);
  auto syntax_tree = minsk::analysis::syntax::syntax_tree::parse(
      std::string{annotated.text()});
  auto compilation = minsk::analysis::compilation{std::move(syntax_tree)};
  auto variables = minsk::analysis::variable_map{};
  auto result = compilation.evaluate(&variables);

  auto diagnostics = annotated_text::unindent_lines(diagnostic_text);
  if (diagnostics.size() != annotated.spans().size()) {
    throw std::runtime_error{"mismatched number of diagnostics and spans"};
  }

  REQUIRE(diagnostics.size() == result.diagnostics().size());

  for (std::size_t i = 0; i < diagnostics.size(); i++) {
    auto expected_message = diagnostics[i];
    auto actual_message = result.diagnostics()[i].message();

    CHECK(expected_message == actual_message);

    auto expected_span = annotated.spans()[i];
    auto actual_span = result.diagnostics()[i].span();

    CHECK(expected_span == actual_span);
  }
}

TEST_CASE("variable declaration reports redeclaration") {
  constexpr std::string_view text = R"(
    {
      var x = 10
      var y = 100
      {
        var x = 10
      }
      var [x] = 5
    }
  )";

  constexpr std::string_view diagnostics = R"(
    Name 'x' is already declared
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("name expression reports undefined") {
  constexpr std::string_view text = "[x] * 10";

  constexpr std::string_view diagnostics = R"(
    Name 'x' is undeclared
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("assignment expression reports undeclared") {
  constexpr std::string_view text = "[x] = 10";

  constexpr std::string_view diagnostics = R"(
    Name 'x' is undeclared
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("assignment expression reports read-only") {
  constexpr std::string_view text = R"(
    {
      let x = 10
      x [=] 20
    }
  )";

  constexpr std::string_view diagnostics = R"(
    Variable 'x' is read-only
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("assignment expression reports cannot convert") {
  constexpr std::string_view text = R"(
    {
      var x = 10
      x = [true]
    }
  )";

  constexpr std::string_view diagnostics = R"(
    Cannot convert type 'boolean' to 'integer'
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("binary operator reports undefined") {
  constexpr std::string_view text = "true [+] 4";

  constexpr std::string_view diagnostics = R"(
    Binary operator '+' isn't defined for types 'boolean' and 'integer'
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("unary operator reports undefined") {
  constexpr std::string_view text = "[+]true";

  constexpr std::string_view diagnostics = R"(
    Unary operator '+' isn't defined for type 'boolean'
  )";

  assert_has_diagnostics(text, diagnostics);
}
