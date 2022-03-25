add_executable(minsk_test source/doctest_main.cpp)
target_link_libraries(minsk_test PRIVATE minsk::minsk doctest::doctest)
target_include_directories(minsk_test PRIVATE "${doctest_SOURCE_DIR}/doctest")

enable_testing()
add_test(NAME minsk_test COMMAND minsk_test)
