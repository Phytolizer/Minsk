add_executable(mc source/main.cpp)

target_link_libraries(mc PRIVATE minsk::minsk rang util::util)
