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

CPMAddPackage(
  NAME fmt
  GITHUB_REPOSITORY fmtlib/fmt
  GIT_TAG 8.1.1
)

CPMAddPackage(
  NAME rang
  GITHUB_REPOSITORY agauniyal/rang
  GIT_TAG v3.2
)

add_subdirectory(util)
add_subdirectory(minsk)
add_subdirectory(mc)
