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
  NAME cmocka
  URL "https://cmocka.org/files/1.1/cmocka-1.1.5.tar.xz"
  URL_MD5 "91f95cd5db88b9b120d191b18d367193"
  VERSION 1.1.5
)

enable_testing()

add_subdirectory(util)
add_subdirectory(styler)
add_subdirectory(minsk)
add_subdirectory(mc)
add_subdirectory(minsk_test)
