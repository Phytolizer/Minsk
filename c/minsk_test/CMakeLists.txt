add_executable(minsk_test "source/analysis/syntax/lexer.c" "source/main.c")
target_include_directories(minsk_test PRIVATE include)
target_link_libraries(minsk_test PRIVATE minsk::minsk Check::check)

add_test(NAME minsk_test COMMAND minsk_test)
