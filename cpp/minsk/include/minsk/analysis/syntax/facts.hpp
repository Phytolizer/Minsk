#ifndef MINSK_FACTS_HPP_606E740B53E945EFAC4DDB27013EB8DE
#define MINSK_FACTS_HPP_606E740B53E945EFAC4DDB27013EB8DE

#include "kind.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace minsk::analysis::syntax::facts {

int binary_operator_precedence(syntax_kind kind);
int unary_operator_precedence(syntax_kind kind);
syntax_kind keyword_kind(std::string_view text);
std::optional<std::string> get_text(syntax_kind kind);

}

#endif // MINSK_FACTS_HPP_606E740B53E945EFAC4DDB27013EB8DE
