#include <minsk-string/string.h>
#include <minsk/code_analysis/text/source_text.h>
#include <minsk/data_structures/buf.h>
#include <tau/tau.h>

#include "minsk-test/tau-ext.h"

TEST(source_text, includes_last_line)
{
  typedef struct
  {
    string_t text;
    size_t expected_line_count;
  } test_case_t;

  static const test_case_t test_data[] = {
    {STRING_REF_C("."), 1},
    {STRING_REF_C(".\r\n"), 2},
    {STRING_REF_C(".\r\n\r\n"), 3},
  };
  typedef BUF_T(test_case_t) test_case_buf_t;
  test_case_buf_t test_cases = BUF_ARRAY(test_case_buf_t, test_data);

  Arena test_arena = {0};

  for (size_t i = 0; i < test_cases.len; ++i)
  {
    minsk_text_source_text_t result =
      minsk_text_source_text_from(test_cases.ptr[i].text, &test_arena);
    EXTCHECK(
      result.lines.len == test_cases.ptr[i].expected_line_count,
      STRING_FMT ": %zu lines != %zu",
      STRING_ARG(test_cases.ptr[i].text),
      result.lines.len,
      test_cases.ptr[i].expected_line_count
    );
  }

  arena_free(&test_arena);
}
