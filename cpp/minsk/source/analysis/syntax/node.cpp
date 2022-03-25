#include "minsk/analysis/syntax/node.hpp"
#include "magic_enum.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/analysis/text/span.hpp"
#include "rang.hpp"
#include <iostream>
#include <string>
#include <string_view>
void minsk::analysis::syntax::syntax_node::pretty_print() const {
  pretty_print(this, std::cout, true, "", true);
}
std::ostream &
minsk::analysis::syntax::syntax_node::write_to(std::ostream &os) const {
  return pretty_print(this, os, false, "", true);
}
std::ostream &minsk::analysis::syntax::syntax_node::pretty_print(
    const minsk::analysis::syntax::syntax_node *n, std::ostream &writer,
    bool is_to_console, std::string indent, bool is_last) {
  if (is_to_console) {
    writer << rang::fg::gray;
  }
  writer << indent;
  std::string marker = is_last ? "└── " : "├── ";
  writer << marker;
  const syntax_token *tok = dynamic_cast<const syntax_token *>(n);
  if (is_to_console) {
    writer << (tok != nullptr ? rang::fg::cyan : rang::fg::blue);
  }
  writer << magic_enum::enum_name(n->kind());
  if (is_to_console) {
    writer << rang::fg::reset;
  }
  if (tok != nullptr && tok->value() != nullptr) {
    writer << ' ';
    tok->value()->print(writer);
  }
  writer << '\n';
  indent += is_last ? "    " : "│   ";
  std::vector<const syntax_node *> children = n->children();
  if (!children.empty()) {
    const syntax_node *last_child = children[children.size() - 1];
    for (const syntax_node *child : children) {
      pretty_print(child, writer, is_to_console, indent, child == last_child);
    }
  }
  return writer;
}

minsk::analysis::text::text_span
minsk::analysis::syntax::syntax_node::span() const {
  auto c = children();
  auto first = c[0];
  auto last = c[c.size() - 1];
  return text::text_span::from_bounds(first->span().start(),
                                      last->span().end());
}

std::ostream &operator<<(std::ostream &os,
                         const minsk::analysis::syntax::syntax_node &node) {
  return node.write_to(os);
}
