#include "minsk/analysis/diagnostic_bag.hpp"
#include "fmt/format.h"
#include "magic_enum.hpp"
#include "minsk/analysis/text/span.hpp"

void minsk::analysis::diagnostic_bag::report(text::text_span span,
                                             std::string &&message) {
  m_diagnostics.emplace_back(span, std::move(message));
}

minsk::analysis::diagnostic_bag::iterator
minsk::analysis::diagnostic_bag::begin() const {
  return m_diagnostics.cbegin();
}

minsk::analysis::diagnostic_bag::iterator
minsk::analysis::diagnostic_bag::end() const {
  return m_diagnostics.end();
}

void minsk::analysis::diagnostic_bag::push_back(const value_type &v) {
  m_diagnostics.push_back(v);
}

bool minsk::analysis::diagnostic_bag::empty() const {
  return m_diagnostics.empty();
}

minsk::analysis::diagnostic_bag::size_type
minsk::analysis::diagnostic_bag::size() const {
  return m_diagnostics.size();
}

void minsk::analysis::diagnostic_bag::report_invalid_int(
    text::text_span span, std::string_view text) {
  report(span, fmt::format("The number '{}' is not a valid int", text));
}

void minsk::analysis::diagnostic_bag::report_bad_character(int position,
                                                           char character) {
  report(text::text_span{position, 1},
         fmt::format("Bad character in input: '{}'", character));
}

void minsk::analysis::diagnostic_bag::report_unexpected_token(
    text::text_span span, syntax::syntax_kind expected_kind,
    syntax::syntax_kind actual_kind) {
  report(span, fmt::format("Expected next token to be <{}>, got <{}> instead",
                           magic_enum::enum_name(expected_kind),
                           magic_enum::enum_name(actual_kind)));
}

void minsk::analysis::diagnostic_bag::report_undefined_binary_operator(
    text::text_span span, std::string_view operator_text,
    runtime::object_kind left_type, runtime::object_kind right_type) {
  report(span, fmt::format(
                   "Binary operator '{}' isn't defined for types '{}' and '{}'",
                   operator_text, magic_enum::enum_name(left_type),
                   magic_enum::enum_name(right_type)));
}

void minsk::analysis::diagnostic_bag::report_undefined_unary_operator(
    text::text_span span, std::string_view operator_text,
    runtime::object_kind operand_type) {
  report(span, fmt::format("Unary operator '{}' isn't defined for type '{}'",
                           operator_text, magic_enum::enum_name(operand_type)));
}

void minsk::analysis::diagnostic_bag::report_undefined_name(
    text::text_span span, std::string_view name) {
  report(span, fmt::format("Name '{}' is undeclared", name));
}

void minsk::analysis::diagnostic_bag::report_variable_already_declared(
    text::text_span span, std::string_view name) {
  report(span, fmt::format("Name '{}' is already declared", name));
}

void minsk::analysis::diagnostic_bag::report_cannot_convert(
    text::text_span span, runtime::object_kind from_type,
    runtime::object_kind to_type) {
  report(span, fmt::format("Cannot convert type '{}' to '{}'",
                           magic_enum::enum_name(from_type),
                           magic_enum::enum_name(to_type)));
}
