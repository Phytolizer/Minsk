add_library(util INTERFACE)
add_library(util::util ALIAS util)

target_include_directories(util INTERFACE include)
target_compile_features(util INTERFACE cxx_std_20)
