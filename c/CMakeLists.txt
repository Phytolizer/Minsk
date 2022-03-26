cmake_minimum_required(VERSION 3.20)

project(
  Minsk
  LANGUAGES C
  HOMEPAGE_URL "https://github.com/Phytolizer/Minsk"
)

set(CMAKE_C_STANDARD 11)

include(cmake/CPM.cmake)

CPMAddPackage(
  NAME sds
  GITHUB_REPOSITORY Phytolizer/sds
  GIT_TAG v2.1.2
)

CPMAddPackage(
  NAME check
  GITHUB_REPOSITORY libcheck/check
  GIT_TAG 0.15.2
)

enable_testing()

add_subdirectory(util)
add_subdirectory(styler)
add_subdirectory(minsk)
add_subdirectory(mc)
add_subdirectory(minsk_test)
