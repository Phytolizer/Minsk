add_library(util source/terminal.cpp)
add_library(util::util ALIAS util)

target_include_directories(util PUBLIC include)
target_compile_features(util PUBLIC cxx_std_20)
