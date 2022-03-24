add_library(
  minsk
  source/analysis/syntax/lexer.cpp
  source/analysis/syntax/nodes/expressions/binary.cpp
  source/analysis/syntax/nodes/expressions/literal.cpp
  source/analysis/syntax/nodes/expressions/parenthesized.cpp
  source/analysis/syntax/nodes/expressions/unary.cpp
  source/analysis/syntax/parser.cpp
  source/analysis/syntax/token.cpp
  source/runtime/object.cpp
)
add_library(minsk::minsk ALIAS minsk)

target_include_directories(minsk PUBLIC include)
target_compile_features(minsk PUBLIC cxx_std_20)
target_link_libraries(minsk PUBLIC magic_enum::magic_enum)
