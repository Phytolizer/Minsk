add_library(util source/line.c)
target_compile_features(util PUBLIC c_std_11)
target_include_directories(util PUBLIC include)

add_library(util::util ALIAS util)
