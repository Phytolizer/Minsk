add_library(
  minsk
  "source/analysis/diagnostic.c"
  "source/analysis/diagnostic_bag.c"
  "source/analysis/evaluator.c"
  "source/analysis/syntax/facts.c"
  "source/analysis/syntax/kind.c"
  "source/analysis/syntax/lexer.c"
  "source/analysis/syntax/node.c"
  "source/analysis/syntax/node/expression/binary.c"
  "source/analysis/syntax/node/expression/literal.c"
  "source/analysis/syntax/node/expression/parenthesized.c"
  "source/analysis/syntax/node/expression/unary.c"
  "source/analysis/syntax/parser.c"
  "source/analysis/syntax/peek_buffer.c"
  "source/analysis/syntax/token.c"
  "source/analysis/text/span.c"
  "source/runtime/object.c"
)
add_library(minsk::minsk ALIAS minsk)
target_include_directories(minsk PUBLIC include)
target_link_libraries(minsk PUBLIC util::util styler::styler)
