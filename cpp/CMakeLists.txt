cmake_minimum_required(VERSION 3.20)

project(
  Minsk
  LANGUAGES CXX
  HOMEPAGE_URL "https://github.com/Phytolizer/Minsk"
)

include(cmake/CPM.cmake)

CPMAddPackage(
  NAME magic_enum
  GITHUB_REPOSITORY Neargye/magic_enum
  GIT_TAG v0.7.3
)

add_subdirectory(minsk)
add_subdirectory(mc)
