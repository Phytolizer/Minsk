add_library(
  minsk "source/analysis/syntax/kind.c" "source/analysis/syntax/lexer.c"
        "source/analysis/syntax/token.c" "source/runtime/object.c"
)
add_library(minsk::minsk ALIAS minsk)
target_include_directories(minsk PUBLIC include)
target_link_libraries(minsk PUBLIC util::util)
