add_library(
  minsk
  source/analysis/evaluator.cpp
  source/analysis/syntax/facts.cpp
  source/analysis/syntax/lexer.cpp
  source/analysis/syntax/node.cpp
  source/analysis/syntax/nodes/expressions/binary.cpp
  source/analysis/syntax/nodes/expressions/literal.cpp
  source/analysis/syntax/nodes/expressions/parenthesized.cpp
  source/analysis/syntax/nodes/expressions/unary.cpp
  source/analysis/syntax/parser.cpp
  source/analysis/syntax/token.cpp
  source/analysis/syntax/tree.cpp
  source/runtime/object.cpp
)
add_library(minsk::minsk ALIAS minsk)

target_include_directories(minsk PUBLIC include)
target_compile_features(minsk PUBLIC cxx_std_20)
if(false)
  target_compile_options(minsk PUBLIC -fsanitize=address,undefined)
  target_link_options(minsk PUBLIC -fsanitize=address,undefined)
endif()
target_link_libraries(minsk PUBLIC magic_enum::magic_enum util::util fmt::fmt)
