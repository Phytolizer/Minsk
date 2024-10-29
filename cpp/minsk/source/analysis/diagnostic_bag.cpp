#include "minsk/analysis/diagnostic_bag.hpp"
#include "fmt/format.h"
#include "magic_enum.hpp"
#include "minsk/analysis/text/span.hpp"

using minsk::analysis::diagnostic;
using minsk::analysis::diagnostic_bag;
using minsk::analysis::text::text_span;
using minsk::runtime::object_kind;

void diagnostic_bag::report(text_span span, std::string &&message) {
  m_diagnostics.emplace_back(span, std::move(message));
}

diagnostic_bag::iterator diagnostic_bag::begin() const {
  return m_diagnostics.cbegin();
}

diagnostic_bag::iterator diagnostic_bag::end() const {
  return m_diagnostics.end();
}

void diagnostic_bag::push_back(const value_type &v) {
  m_diagnostics.push_back(v);
}

const diagnostic &diagnostic_bag::operator[](size_type i) const {
  return m_diagnostics[i];
}

bool diagnostic_bag::empty() const { return m_diagnostics.empty(); }

diagnostic_bag::size_type diagnostic_bag::size() const {
  return m_diagnostics.size();
}

void diagnostic_bag::report_invalid_int(text_span span, std::string_view text) {
  report(span, fmt::format("The number '{}' is not a valid int", text));
}

void diagnostic_bag::report_bad_character(int position, char character) {
  report(text_span{position, 1},
         fmt::format("Bad character in input: '{}'", character));
}

void diagnostic_bag::report_unexpected_token(text_span span,
                                             syntax::syntax_kind expected_kind,
                                             syntax::syntax_kind actual_kind) {
  report(span, fmt::format("Expected next token to be <{}>, got <{}> instead",
                           magic_enum::enum_name(expected_kind),
                           magic_enum::enum_name(actual_kind)));
}

void diagnostic_bag::report_undefined_binary_operator(
    text_span span, std::string_view operator_text, object_kind left_type,
    object_kind right_type) {
  report(span, fmt::format(
                   "Binary operator '{}' isn't defined for types '{}' and '{}'",
                   operator_text, magic_enum::enum_name(left_type),
                   magic_enum::enum_name(right_type)));
}

void diagnostic_bag::report_undefined_unary_operator(
    text_span span, std::string_view operator_text, object_kind operand_type) {
  report(span, fmt::format("Unary operator '{}' isn't defined for type '{}'",
                           operator_text, magic_enum::enum_name(operand_type)));
}

void diagnostic_bag::report_undefined_name(text_span span,
                                           std::string_view name) {
  report(span, fmt::format("Name '{}' is undeclared", name));
}

void diagnostic_bag::report_variable_already_declared(text_span span,
                                                      std::string_view name) {
  report(span, fmt::format("Name '{}' is already declared", name));
}

void diagnostic_bag::report_cannot_convert(text_span span,
                                           object_kind from_type,
                                           object_kind to_type) {
  report(span, fmt::format("Cannot convert type '{}' to '{}'",
                           magic_enum::enum_name(from_type),
                           magic_enum::enum_name(to_type)));
}

void diagnostic_bag::report_cannot_assign(text_span span,
                                          std::string_view name) {
  report(span, fmt::format("Variable '{}' is read-only", name));
}
