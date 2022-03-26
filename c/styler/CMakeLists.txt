add_library(styler source/styler.c)
add_library(styler::styler ALIAS styler)
target_include_directories(styler PUBLIC include)
