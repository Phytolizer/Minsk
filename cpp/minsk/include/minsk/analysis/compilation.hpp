#ifndef MINSK_ANALYSIS_COMPILATION_HPP
#define MINSK_ANALYSIS_COMPILATION_HPP

#include "minsk/analysis/binding/scope/global.hpp"
#include "minsk/analysis/evaluation_result.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/analysis/variable_map.hpp"
#include "minsk/runtime/object.hpp"

namespace minsk::analysis {

class compilation final {
  syntax::syntax_tree m_syntax;
  binding::bound_global_scope m_global_scope;

public:
  explicit compilation(syntax::syntax_tree syntax);

  const syntax::syntax_tree &syntax() const;

  evaluation_result evaluate(variable_map *variables) const;
};

} // namespace minsk::analysis

#endif // MINSK_ANALYSIS_COMPILATION_HPP
