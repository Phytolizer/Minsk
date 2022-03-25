#include "minsk/analysis/evaluation_result.hpp"

minsk::analysis::evaluation_result::evaluation_result(runtime::object_ptr value)
    : m_value(std::move(value)) {}

minsk::analysis::evaluation_result::evaluation_result(
    diagnostic_bag &&diagnostics)
    : m_diagnostics(std::move(diagnostics)) {}

const minsk::runtime::object *
minsk::analysis::evaluation_result::value() const {
  return m_value.get();
}

const minsk::analysis::diagnostic_bag &
minsk::analysis::evaluation_result::diagnostics() const {
  return m_diagnostics;
}
