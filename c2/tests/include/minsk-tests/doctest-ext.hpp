#pragma once

#include <algorithm>
#include <string>

#define DOCTEST_VALUE_PARAMETERIZED_DATA(data, container)         \
 static std::size_t _doctest_subcase_idx = 0;                     \
 std::foreach (                                                   \
   container.begin(),                                             \
   container.end(),                                               \
   [&](const auto & in)                                           \
   {                                                              \
   DOCTEST_SUBCASE((std::string{#container "["} +                 \
                    std::to_string(_doctest_subcase_idx++) + "]") \
                     .c_str())                                    \
   {                                                              \
    data = in;                                                    \
   }                                                              \
   }                                                              \
 );                                                               \
 _doctest_subcase_idx = 0
