add_executable(minsk_test source/main.c)
target_link_libraries(minsk_test PRIVATE cmocka)

add_test(NAME minsk_test COMMAND minsk_test)
