#ifndef MINSK_ANALYSIS_COMPILATION_HPP
#define MINSK_ANALYSIS_COMPILATION_HPP

#include "minsk/analysis/evaluation_result.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/runtime/object.hpp"
namespace minsk::analysis {

class compilation final {
  syntax::syntax_tree m_syntax;

public:
  explicit compilation(syntax::syntax_tree syntax);

  const syntax::syntax_tree &syntax() const;

  evaluation_result evaluate() const;
};

} // namespace minsk::analysis

#endif // MINSK_ANALYSIS_COMPILATION_HPP
