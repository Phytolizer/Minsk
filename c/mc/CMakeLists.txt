add_executable(mc source/main.c)
target_link_libraries(mc PRIVATE util::util minsk::minsk styler::styler)
