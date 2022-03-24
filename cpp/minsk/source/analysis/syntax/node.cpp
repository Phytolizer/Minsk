#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <iostream>
void minsk::analysis::syntax::node::pretty_print() const {
  pretty_print(this, std::cout, true, "", true);
}
void minsk::analysis::syntax::node::pretty_print(
    const minsk::analysis::syntax::node *n, std::ostream &writer,
    bool is_to_console, std::string indent, bool is_last) {
  writer << indent;
  std::string marker = is_last ? "└── " : "├── ";
  writer << marker;
  writer << magic_enum::enum_name(n->kind());
  const syntax_token *tok = dynamic_cast<const syntax_token *>(n);
  if (tok != nullptr && tok->value() != nullptr) {
    writer << ' ';
    tok->value()->print(writer);
  }
  writer << '\n';
  indent += is_last ? "    " : "│   ";
  std::vector<const node *> children = n->children();
  if (!children.empty()) {
    const node *last_child = children[children.size() - 1];
    for (const node *child : children) {
      pretty_print(child, writer, is_to_console, indent, child == last_child);
    }
  }
}
