#include "minsk/analysis/evaluation_result.hpp"

using minsk::analysis::diagnostic_bag;
using minsk::analysis::evaluation_result;
using minsk::runtime::object;
using minsk::runtime::object_ptr;

evaluation_result::evaluation_result(object_ptr value)
    : m_value(std::move(value)) {}

evaluation_result::evaluation_result(diagnostic_bag &&diagnostics)
    : m_diagnostics(std::move(diagnostics)) {}

const object *evaluation_result::value() const { return m_value.get(); }

const diagnostic_bag &evaluation_result::diagnostics() const {
  return m_diagnostics;
}
