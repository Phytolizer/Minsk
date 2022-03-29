#ifndef MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C
#define MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C

#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/assignment.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include "minsk/analysis/binding/nodes/expressions/variable.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include "minsk/analysis/binding/nodes/statements/block.hpp"
#include "minsk/analysis/binding/nodes/statements/expression.hpp"
#include "minsk/analysis/binding/nodes/statements/for.hpp"
#include "minsk/analysis/binding/nodes/statements/if.hpp"
#include "minsk/analysis/binding/nodes/statements/variable.hpp"
#include "minsk/analysis/binding/nodes/statements/while.hpp"
#include "minsk/analysis/variable_map.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>

namespace minsk::analysis {

class evaluator final {
  const binding::bound_statement *m_root;
  variable_map *m_variables;
  runtime::object_ptr m_last_value;

  void evaluate_statement(const binding::bound_statement *root);
  void evaluate_block_statement(const binding::bound_block_statement *root);
  void evaluate_expression_statement(
      const binding::bound_expression_statement *root);
  void evaluate_if_statement(const binding::bound_if_statement *root);
  void evaluate_for_statement(const binding::bound_for_statement *root);
  void evaluate_variable_declaration(
      const binding::bound_variable_declaration *root);
  void evaluate_while_statement(const binding::bound_while_statement *root);
  runtime::object_ptr
  evaluate_expression(const binding::bound_expression *root);
  runtime::object_ptr evaluate_assignment_expression(
      const binding::bound_assignment_expression *root);
  runtime::object_ptr
  evaluate_binary_expression(const binding::bound_binary_expression *root);
  runtime::object_ptr
  evaluate_literal_expression(const binding::bound_literal_expression *root);
  runtime::object_ptr
  evaluate_unary_expression(const binding::bound_unary_expression *root);
  runtime::object_ptr
  evaluate_variable_expression(const binding::bound_variable_expression *root);

public:
  evaluator(const binding::bound_statement *root, variable_map *variables);
  runtime::object_ptr evaluate();
};

} // namespace minsk::analysis

#endif // MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C
