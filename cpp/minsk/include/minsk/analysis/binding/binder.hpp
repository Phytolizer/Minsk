#ifndef MINSK_ANALYSIS_BINDING_BINDER_HPP
#define MINSK_ANALYSIS_BINDING_BINDER_HPP

#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include "minsk/analysis/binding/scope.hpp"
#include "minsk/analysis/binding/scope/global.hpp"
#include "minsk/analysis/compilation.hpp"
#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/expressions/assignment.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/name.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/nodes/statements/block.hpp"
#include "minsk/analysis/syntax/nodes/statements/expression.hpp"
#include "minsk/analysis/syntax/nodes/statements/for.hpp"
#include "minsk/analysis/syntax/nodes/statements/if.hpp"
#include "minsk/analysis/syntax/nodes/statements/variable.hpp"
#include "minsk/analysis/syntax/nodes/statements/while.hpp"
#include "minsk/analysis/syntax/nodes/unit.hpp"
#include "minsk/analysis/variable_map.hpp"
#include <memory>

namespace minsk::analysis::binding {

class binder final {
  diagnostic_bag m_diagnostics;
  std::unique_ptr<bound_scope> m_scope;
  std::unique_ptr<bound_statement>
  bind_block_statement(const syntax::block_statement_syntax *syntax);
  std::unique_ptr<bound_statement>
  bind_expression_statement(const syntax::expression_statement_syntax *syntax);
  std::unique_ptr<bound_statement>
  bind_for_statement(const syntax::for_statement_syntax *syntax);
  std::unique_ptr<bound_statement>
  bind_if_statement(const syntax::if_statement_syntax *syntax);
  std::unique_ptr<bound_statement>
  bind_variable_declaration(const syntax::variable_declaration_syntax *syntax);
  std::unique_ptr<bound_statement>
  bind_while_statement(const syntax::while_statement_syntax *syntax);
  std::unique_ptr<bound_expression> bind_expression(
      const syntax::expression_syntax *syntax,
      runtime::object_kind required_type = runtime::object_kind::null);
  std::unique_ptr<bound_expression> bind_assignment_expression(
      const syntax::assignment_expression_syntax *syntax);
  std::unique_ptr<bound_expression>
  bind_binary_expression(const syntax::binary_expression_syntax *syntax);
  std::unique_ptr<bound_expression>
  bind_literal_expression(const syntax::literal_expression_syntax *syntax);
  std::unique_ptr<bound_expression>
  bind_name_expression(const syntax::name_expression_syntax *syntax);
  std::unique_ptr<bound_expression> bind_parenthesized_expression(
      const syntax::parenthesized_expression_syntax *syntax);
  std::unique_ptr<bound_expression>
  bind_unary_expression(const syntax::unary_expression_syntax *syntax);
  static std::unique_ptr<bound_scope>
  create_parent_scope(bound_global_scope *previous);

public:
  explicit binder(std::unique_ptr<bound_scope> parent);
  static bound_global_scope
  bind_global_scope(bound_global_scope *previous,
                    const syntax::compilation_unit_syntax *syntax);
  std::unique_ptr<bound_statement>
  bind_statement(const syntax::statement_syntax *syntax);
  const diagnostic_bag &diagnostics() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_BINDER_HPP
