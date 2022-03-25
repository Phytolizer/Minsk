#include "minsk/analysis/binding/nodes/statements/block.hpp"
#include "minsk/analysis/binding/kind.hpp"

minsk::analysis::binding::bound_block_statement::bound_block_statement(
    std::vector<std::unique_ptr<bound_statement>> &&statements)
    : m_statements(std::move(statements)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_block_statement::kind() const {
  return bound_node_kind::block_statement;
}

const std::vector<std::unique_ptr<minsk::analysis::binding::bound_statement>> &
minsk::analysis::binding::bound_block_statement::statements() const {
  return m_statements;
}
