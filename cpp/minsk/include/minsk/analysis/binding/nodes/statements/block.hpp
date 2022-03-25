#ifndef MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_BLOCK_HPP
#define MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_BLOCK_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include <memory>
#include <vector>
namespace minsk::analysis::binding {

class bound_block_statement final : public bound_statement {
  std::vector<std::unique_ptr<bound_statement>> m_statements;

public:
  explicit bound_block_statement(
      std::vector<std::unique_ptr<bound_statement>> &&statements);

  bound_node_kind kind() const override;

  const std::vector<std::unique_ptr<bound_statement>> &statements() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_BLOCK_HPP
