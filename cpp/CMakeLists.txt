cmake_minimum_required(VERSION 3.20)

project(
  Minsk
  LANGUAGES CXX
  HOMEPAGE_URL "https://github.com/Phytolizer/Minsk"
)

set(CMAKE_CXX_STANDARD 20)

include(cmake/CPM.cmake)

CPMAddPackage(
  NAME magic_enum
  GITHUB_REPOSITORY Neargye/magic_enum
  GIT_TAG v0.9.6
)

CPMAddPackage(
  NAME fmt
  GITHUB_REPOSITORY fmtlib/fmt
  GIT_TAG 11.0.2
)

CPMAddPackage(
  NAME rang
  GITHUB_REPOSITORY agauniyal/rang
  GIT_TAG v3.2
)

CPMAddPackage(
  NAME doctest
  GITHUB_REPOSITORY doctest/doctest
  GIT_TAG v2.4.11
)

enable_testing()

add_subdirectory(util)
add_subdirectory(minsk)
add_subdirectory(minsk_test)
add_subdirectory(mc)
