#ifndef MINSK_DIAGNOSTIC_BAG_HPP_28AA0BAA38744079921B472E7B98470E
#define MINSK_DIAGNOSTIC_BAG_HPP_28AA0BAA38744079921B472E7B98470E

#include "minsk/analysis/diagnostic.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/text/span.hpp"
#include "minsk/runtime/object.hpp"
#include <string>
#include <vector>

namespace minsk::analysis {

class diagnostic_bag final {
  std::vector<diagnostic> m_diagnostics;

  void report(text::text_span span, std::string &&message);

public:
  using value_type = diagnostic;
  using size_type = std::vector<diagnostic>::size_type;
  using iterator = std::vector<diagnostic>::const_iterator;

  diagnostic_bag() = default;

  iterator begin() const;
  iterator end() const;

  void push_back(const value_type &v);

  const diagnostic &operator[](size_type i) const;

  bool empty() const;
  size_type size() const;

  void report_invalid_int(text::text_span span, std::string_view text);
  void report_bad_character(int position, char character);
  void report_unexpected_token(text::text_span span,
                               syntax::syntax_kind expected_kind,
                               syntax::syntax_kind actual_kind);
  void report_undefined_binary_operator(text::text_span span,
                                        std::string_view operator_text,
                                        runtime::object_kind left_type,
                                        runtime::object_kind right_type);
  void report_undefined_unary_operator(text::text_span span,
                                       std::string_view operator_text,
                                       runtime::object_kind operand_type);
  void report_undefined_name(text::text_span span, std::string_view name);
  void report_variable_already_declared(text::text_span span,
                                        std::string_view name);
  void report_cannot_convert(text::text_span span,
                             runtime::object_kind from_type,
                             runtime::object_kind to_type);
  void report_cannot_assign(text::text_span span, std::string_view name);
};

} // namespace minsk::analysis

#endif // MINSK_DIAGNOSTIC_BAG_HPP_28AA0BAA38744079921B472E7B98470E
