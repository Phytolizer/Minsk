add_executable(
  minsk_test
  "source/analysis/evaluator.c"
  "source/analysis/syntax/asserting_iterator.c"
  "source/analysis/syntax/kind_set.c"
  "source/analysis/syntax/kind_vector.c"
  "source/analysis/syntax/lexer.c"
  "source/analysis/syntax/node_vector.c"
  "source/analysis/syntax/parser.c"
  "source/esc.c"
  "source/main.c"
)
target_include_directories(minsk_test PRIVATE include)
find_package(Threads REQUIRED)
target_link_libraries(
  minsk_test PRIVATE minsk::minsk Check::check Threads::Threads
)

add_test(NAME minsk_test COMMAND minsk_test)
