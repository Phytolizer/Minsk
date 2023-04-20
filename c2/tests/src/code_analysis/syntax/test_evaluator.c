#include <arena.h>
#include <inttypes.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/compilation.h>
#include <minsk/code_analysis/diagnostic.h>
#include <minsk/code_analysis/diagnostic_bag.h>
#include <minsk/code_analysis/syntax/tree.h>
#include <minsk/code_analysis/variable_map.h>
#include <minsk/data_structures/buf.h>
#include <minsk/runtime/object.h>
#include <stdbool.h>
#include <stddef.h>
#include <tau/tau.h>

#include "minsk-test/tau-ext.h"

static void
test_correct_evaluation(string_t input, minsk_object_t expected)
{
  Arena test_arena = {0};
  minsk_syntax_tree_t tree = minsk_syntax_tree_parse(&test_arena, input);
  minsk_compilation_t compilation = minsk_compilation_new(&test_arena, tree);
  minsk_variable_map_t variables = minsk_variable_map_new(&test_arena);
  minsk_evaluation_result_t result =
    minsk_compilation_evaluate(&compilation, &variables);
  if (!result.success)
  {
    string_t diagnostics =
      string_dup_arena(&test_arena, STRING_REF("got diagnostics:"));
    for (size_t i = 0; i < result.diagnostics.len; i++)
    {
      string_push_arena(&test_arena, &diagnostics, '\n');
      string_append_arena(
        &test_arena,
        &diagnostics,
        result.diagnostics.ptr[i].message
      );
    }
    EXTREQUIRE(
      false,
      STRING_FMT ": " STRING_FMT,
      STRING_ARG(input),
      STRING_ARG(diagnostics)
    );
  }
  EXTREQUIRE(
    result.value.type == expected.type,
    STRING_FMT ": got wrong type (" STRING_FMT " != " STRING_FMT ")",
    STRING_ARG(input),
    STRING_ARG(minsk_object_type_display_name(&test_arena, result.value.type)),
    STRING_ARG(minsk_object_type_display_name(&test_arena, expected.type))
  );
  switch (expected.type)
  {
    case MINSK_OBJECT_TYPE_BOOLEAN:
      EXTCHECK(
        result.value.boolean == expected.boolean,
        STRING_FMT ": expected " STRING_FMT ", got " STRING_FMT,
        STRING_ARG(input),
        STRING_ARG(
          result.value.boolean ? STRING_REF("true") : STRING_REF("false")
        ),
        STRING_ARG(expected.boolean ? STRING_REF("true") : STRING_REF("false"))
      );
      break;
    case MINSK_OBJECT_TYPE_INTEGER:
      EXTCHECK(
        result.value.integer == expected.integer,
        STRING_FMT ": expected %" PRId64 ", got %" PRId64,
        STRING_ARG(input),
        result.value.integer,
        expected.integer
      );
      break;
    default: EXTREQUIRE(false, "unreachable");
  }
  arena_free(&test_arena);
}

TEST(evaluator, correct_evaluation)
{
  typedef struct
  {
    string_t input;
    minsk_object_t expected;
  } test_t;

#define T(i, x) {.input = STRING_REF(i), .expected = (x)}

  test_t const test_data[] = {
    T("1", MINSK_OBJECT_INTEGER(1)),
    T("+1", MINSK_OBJECT_INTEGER(1)),
    T("-1", MINSK_OBJECT_INTEGER(-1)),
    T("14 + 12", MINSK_OBJECT_INTEGER(26)),
    T("12 - 3", MINSK_OBJECT_INTEGER(9)),
    T("4 * 2", MINSK_OBJECT_INTEGER(8)),
    T("9 / 3", MINSK_OBJECT_INTEGER(3)),
    T("(10)", MINSK_OBJECT_INTEGER(10)),
    T("12 == 3", MINSK_OBJECT_BOOLEAN(false)),
    T("3 == 3", MINSK_OBJECT_BOOLEAN(true)),
    T("12 != 3", MINSK_OBJECT_BOOLEAN(true)),
    T("3 != 3", MINSK_OBJECT_BOOLEAN(false)),
    T("false == false", MINSK_OBJECT_BOOLEAN(true)),
    T("true == false", MINSK_OBJECT_BOOLEAN(false)),
    T("false != false", MINSK_OBJECT_BOOLEAN(false)),
    T("true != false", MINSK_OBJECT_BOOLEAN(true)),
    T("true", MINSK_OBJECT_BOOLEAN(true)),
    T("false", MINSK_OBJECT_BOOLEAN(false)),
    T("!true", MINSK_OBJECT_BOOLEAN(false)),
    T("!false", MINSK_OBJECT_BOOLEAN(true)),
    T("(a = 10) * a", MINSK_OBJECT_INTEGER(100)),
  };
#undef T
  typedef BUF_T(test_t) test_buf_t;
  test_buf_t tests = BUF_ARRAY(test_buf_t, test_data);

  for (size_t i = 0; i < tests.len; i++)
  {
    test_correct_evaluation(tests.ptr[i].input, tests.ptr[i].expected);
  }
}
