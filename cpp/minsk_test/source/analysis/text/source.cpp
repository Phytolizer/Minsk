#include "minsk/analysis/text/source.hpp"
#include "doctest.h"
#include "minsk_test/parametrize.hpp"
#include <string_view>

TEST_CASE("includes last line") {
  struct source_text_test final {
    std::string_view text;
    int expected_line_count;
  };

  auto data = source_text_test{};
  constexpr auto tests = std::array{
      source_text_test{".", 1},
      source_text_test{".\r\n", 2},
      source_text_test{".\r\n\r\n", 3},
  };

  DOCTEST_VALUE_PARAMETERIZED_DATA(data, tests);

  auto source_text =
      minsk::analysis::text::source_text::from(std::string{data.text});
  CHECK(source_text.lines().size() == data.expected_line_count);
}
