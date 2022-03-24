#ifndef MINSK_KIND_HPP
#define MINSK_KIND_HPP

#include <magic_enum.hpp>

namespace minsk::analysis::syntax {

enum class syntax_kind {
  bad_token,
  end_of_file_token,

  whitespace_token,
  number_token,

  plus_token,
  minus_token,
  star_token,
  slash_token,
  open_parenthesis_token,
  close_parenthesis_token,
};

}

#endif // MINSK_KIND_HPP
