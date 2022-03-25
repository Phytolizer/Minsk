#ifndef MINSK_ANALYSIS_VARIABLE_MAP_HPP
#define MINSK_ANALYSIS_VARIABLE_MAP_HPP

#include "minsk/runtime/object.hpp"
#include <string>
#include <unordered_map>

namespace minsk::analysis {

using variable_map = std::unordered_map<std::string, runtime::object_ptr>;

}

#endif // MINSK_ANALYSIS_VARIABLE_MAP_HPP
