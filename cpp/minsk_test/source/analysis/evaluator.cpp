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
      evaluation_test{"~1", std::make_unique<integer>(-2)},
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
      evaluation_test{"1 | 2", std::make_unique<integer>(3)},
      evaluation_test{"1 | 0", std::make_unique<integer>(1)},
      evaluation_test{"2 & 3", std::make_unique<integer>(2)},
      evaluation_test{"2 & 1", std::make_unique<integer>(0)},
      evaluation_test{"1 ^ 3", std::make_unique<integer>(2)},
      evaluation_test{"1 ^ 2", std::make_unique<integer>(3)},
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
      evaluation_test{"true && false", std::make_unique<boolean>(false)},
      evaluation_test{"true && true", std::make_unique<boolean>(true)},
      evaluation_test{"true | false", std::make_unique<boolean>(true)},
      evaluation_test{"false | false", std::make_unique<boolean>(false)},
      evaluation_test{"true & false", std::make_unique<boolean>(false)},
      evaluation_test{"true & true", std::make_unique<boolean>(true)},
      evaluation_test{"true ^ false", std::make_unique<boolean>(true)},
      evaluation_test{"true ^ true", std::make_unique<boolean>(false)},
      evaluation_test{"false ^ false", std::make_unique<boolean>(false)},
      evaluation_test{"var a = 10", std::make_unique<integer>(10)},
      evaluation_test{R"(
        {
          var a = 10
          (a * a)
        }
      )",
                      std::make_unique<integer>(100)},
      evaluation_test{R"(
        {
          var a = 0
          (a = 10) * a
        }
      )",
                      std::make_unique<integer>(100)},
      evaluation_test{R"(
        {
          var a = 10
          if false
            a = 20
          a
        }
      )",
                      std::make_unique<integer>(10)},
      evaluation_test{R"(
        {
          var a = 10
          if true
            a = 20
          a
        }
      )",
                      std::make_unique<integer>(20)},
      evaluation_test{R"(
        {
          var a = 0
          if true
            a = 20
          else
            a = 10
          a
        }
      )",
                      std::make_unique<integer>(20)},
      evaluation_test{R"(
        {
          var a = 0
          if false
            a = 20
          else
            a = 10
          a
        }
      )",
                      std::make_unique<integer>(10)},
      evaluation_test{R"(
        {
          var a = 0
          while a < 10
            a = a + 1
          a
        }
      )",
                      std::make_unique<integer>(10)},
      evaluation_test{
          R"(
        {
          for i = 1 to 10
            i
        }
      )",
          std::make_unique<integer>(9)},
  };

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, evaluator_tests);

  auto syntax_tree =
      minsk::analysis::syntax::syntax_tree::parse(std::string{data.text});
  auto compilation = minsk::analysis::compilation{std::move(syntax_tree)};
  auto variables = minsk::analysis::variable_map{};
  auto result = compilation.evaluate(&variables);
  for (const auto &diagnostic : result.diagnostics()) {
    FAIL_CHECK(diagnostic.message());
  }
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

TEST_CASE("if statement requires boolean condition") {
  constexpr std::string_view text = R"(
    if [1]
      1
  )";

  constexpr std::string_view diagnostics = R"(
    Cannot convert type 'integer' to 'boolean'
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("while statement requires boolean condition") {
  constexpr std::string_view text = R"(
    while [1]
      1
  )";

  constexpr std::string_view diagnostics = R"(
    Cannot convert type 'integer' to 'boolean'
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("for statement requires integer range") {
  constexpr std::string_view text = R"(
    for i = [true] to 10
      i
  )";

  constexpr std::string_view diagnostics = R"(
    Cannot convert type 'boolean' to 'integer'
  )";

  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("block statement does not loop infinitely") {
  constexpr std::string_view text = R"(
    {
    [)][]
  )";
  constexpr std::string_view diagnostics = R"(
    Expected next token to be <identifier_token>, got <close_parenthesis_token> instead
    Expected next token to be <close_brace_token>, got <end_of_file_token> instead
  )";
  assert_has_diagnostics(text, diagnostics);
}

TEST_CASE("name expression reports no error when manufactured") {
  constexpr std::string_view text = "[]";
  constexpr std::string_view diagnostics = R"(
    Expected next token to be <identifier_token>, got <end_of_file_token> instead
  )";
  assert_has_diagnostics(text, diagnostics);
}
