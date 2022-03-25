#ifndef MINSK_ANALYSIS_VARIABLE_MAP_HPP
#define MINSK_ANALYSIS_VARIABLE_MAP_HPP

#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"
#include <string>
#include <unordered_map>

namespace minsk::analysis {

using variable_map = std::unordered_map<variable_symbol, runtime::object_ptr>;

}

#endif // MINSK_ANALYSIS_VARIABLE_MAP_HPP
