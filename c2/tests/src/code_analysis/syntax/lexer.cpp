#include <iomanip>
#include <minsk/code_analysis/syntax/kind.h>
#include <vector>
extern "C"
{
#include <arena.h>
#include <minsk/code_analysis/syntax/lexer.h>
#include <minsk/code_analysis/syntax/tree.h>
}
#include <array>
#include <doctest.h>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <memory>
#include <string_view>

#include "minsk-tests/doctest-ext.hpp"

namespace
{
struct SimpleToken
{
  minsk_syntax_kind_t kind;
  std::string_view text;

  friend std::ostream &
  operator<<(std::ostream & os, const SimpleToken & t)
  {
    fmt::print(os, "({} {:?})", magic_enum::enum_name(t.kind), t.text);
    return os;
  }
};

constexpr auto TOKENS = std::array{
  SimpleToken{MINSK_SYNTAX_KIND_PLUS_TOKEN, "+"},
  SimpleToken{MINSK_SYNTAX_KIND_MINUS_TOKEN, "-"},
  SimpleToken{MINSK_SYNTAX_KIND_STAR_TOKEN, "*"},
  SimpleToken{MINSK_SYNTAX_KIND_SLASH_TOKEN, "/"},
  SimpleToken{MINSK_SYNTAX_KIND_BANG_TOKEN, "!"},
  SimpleToken{MINSK_SYNTAX_KIND_EQUALS_TOKEN, "="},
  SimpleToken{MINSK_SYNTAX_KIND_AMPERSAND_AMPERSAND_TOKEN, "&&"},
  SimpleToken{MINSK_SYNTAX_KIND_PIPE_PIPE_TOKEN, "||"},
  SimpleToken{MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN, "=="},
  SimpleToken{MINSK_SYNTAX_KIND_BANG_EQUALS_TOKEN, "!="},
  SimpleToken{MINSK_SYNTAX_KIND_OPEN_PARENTHESIS_TOKEN, "("},
  SimpleToken{MINSK_SYNTAX_KIND_CLOSE_PARENTHESIS_TOKEN, ")"},
  SimpleToken{MINSK_SYNTAX_KIND_FALSE_KEYWORD, "false"},
  SimpleToken{MINSK_SYNTAX_KIND_TRUE_KEYWORD, "true"},
  SimpleToken{MINSK_SYNTAX_KIND_NUMBER_TOKEN, "1"},
  SimpleToken{MINSK_SYNTAX_KIND_NUMBER_TOKEN, "123"},
  SimpleToken{MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN, "a"},
  SimpleToken{MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN, "abc"},
};

constexpr auto SEPARATORS = std::array{
  SimpleToken{MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, " "},
  SimpleToken{MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "  "},
  SimpleToken{MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\r"},
  SimpleToken{MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\n"},
  SimpleToken{MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\r\n"},
};

constexpr bool
requires_separator(minsk_syntax_kind_t k1, minsk_syntax_kind_t k2)
{
  std::string_view k1_str = magic_enum::enum_name(k1);
  std::string_view k2_str = magic_enum::enum_name(k2);
  constexpr std::string_view keyword_text = "_KEYWORD";
  bool k1_is_keyword = std::find_end(
                         k1_str.begin(),
                         k1_str.end(),
                         keyword_text.begin(),
                         keyword_text.end()
                       ) != k1_str.end();
  bool k2_is_keyword = std::find_end(
                         k2_str.begin(),
                         k2_str.end(),
                         keyword_text.begin(),
                         keyword_text.end()
                       ) != k2_str.end();

  return (
    ((k1_is_keyword || k1 == MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN) &&
     (k2_is_keyword || k2 == MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN ||
      k2 == MINSK_SYNTAX_KIND_NUMBER_TOKEN)) ||
    (k1 == MINSK_SYNTAX_KIND_NUMBER_TOKEN &&
     k2 == MINSK_SYNTAX_KIND_NUMBER_TOKEN) ||
    ((k1 == MINSK_SYNTAX_KIND_BANG_TOKEN || k1 == MINSK_SYNTAX_KIND_EQUALS_TOKEN
     ) &&
     (k2 == MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN ||
      k2 == MINSK_SYNTAX_KIND_EQUALS_TOKEN))
  );
}

struct ArenaDeleter
{
  void
  operator()(Arena * arena)
  {
    arena_free(arena);
  }
};
}  // namespace

doctest::String
toString(minsk_syntax_kind_t value)
{
  return magic_enum::enum_name(value).data();
}

namespace doctest
{
template <>
struct StringMaker<std::string_view>
{
  static String
  convert(std::string_view in)
  {
    return fmt::format("{:?}", in).c_str();
  }
};
}  // namespace doctest

TEST_CASE("Lexer lexes token")
{
  SimpleToken data;
  for (const SimpleToken & t :
       []
       {
         std::vector<SimpleToken> result;
         result.reserve(TOKENS.size() + SEPARATORS.size());
         std::copy(TOKENS.begin(), TOKENS.end(), std::back_inserter(result));
         std::copy(
           SEPARATORS.begin(),
           SEPARATORS.end(),
           std::back_inserter(result)
         );
         return result;
       }())
  {
    SUBCASE(fmt::to_string(fmt::streamed(t)).c_str())
    {
      data = t;
    };
  }

  Arena a{};
  std::unique_ptr<Arena, ArenaDeleter> arena{&a};
  minsk_syntax_token_buf_t tokens = minsk_syntax_tree_parse_tokens(
    arena.get(),
    STRING_REF_DATA(data.text.data(), data.text.length())
  );

  REQUIRE(tokens.len == 1);
  minsk_syntax_token_t token = tokens.ptr[0];
  REQUIRE_EQ(token.kind, data.kind);
  REQUIRE_EQ(std::string_view{token.text.data, token.text.length}, data.text);
}

TEST_CASE("Lexer lexes token pair")
{
  SimpleToken data1, data2;
  for (const SimpleToken & t1 : TOKENS)
  {
    for (const SimpleToken & t2 : TOKENS)
    {
      if (!requires_separator(t1.kind, t2.kind))
      {
        SUBCASE(
          fmt::format("{} {}", fmt::streamed(t1), fmt::streamed(t2)).c_str()
        )
        {
          data1 = t1;
          data2 = t2;
        };
      }
    }
  }

  Arena a{};
  std::unique_ptr<Arena, ArenaDeleter> arena{&a};
  auto text = fmt::format("{}{}", data1.text, data2.text);
  minsk_syntax_token_buf_t tokens = minsk_syntax_tree_parse_tokens(
    arena.get(),
    STRING_REF_DATA(text.data(), text.length())
  );

  REQUIRE(tokens.len == 2);
  minsk_syntax_token_t t1 = tokens.ptr[0];
  minsk_syntax_token_t t2 = tokens.ptr[1];
  REQUIRE_EQ(t1.kind, data1.kind);
  REQUIRE_EQ(std::string_view{t1.text.data, t1.text.length}, data1.text);
  REQUIRE_EQ(t2.kind, data2.kind);
  REQUIRE_EQ(std::string_view{t2.text.data, t2.text.length}, data2.text);
}
