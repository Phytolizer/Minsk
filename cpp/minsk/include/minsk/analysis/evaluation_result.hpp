#ifndef MINSK_ANALYSIS_EVALUATION_RESULT_HPP
#define MINSK_ANALYSIS_EVALUATION_RESULT_HPP

#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/runtime/object.hpp"

namespace minsk::analysis {

class evaluation_result final {
  runtime::object_ptr m_value;
  diagnostic_bag m_diagnostics;

public:
  explicit evaluation_result(runtime::object_ptr value);
  explicit evaluation_result(diagnostic_bag &&diagnostics);

  const runtime::object *value() const;
  const diagnostic_bag &diagnostics() const;
};

} // namespace minsk::analysis

#endif // MINSK_ANALYSIS_EVALUATION_RESULT_HPP
