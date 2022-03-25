add_executable(
  minsk_test
  source/analysis/evaluator.cpp source/analysis/syntax/facts.cpp
  source/analysis/syntax/lexer.cpp source/analysis/syntax/parser.cpp
  source/analysis/text/source.cpp source/doctest_main.cpp
)
target_link_libraries(minsk_test PRIVATE minsk::minsk doctest::doctest)
target_include_directories(
  minsk_test PRIVATE include "${doctest_SOURCE_DIR}/doctest"
)

add_test(NAME minsk_test COMMAND minsk_test)
