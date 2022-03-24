#ifndef MINSK_OBJECT_HPP
#define MINSK_OBJECT_HPP

#include <memory>

namespace minsk::runtime {

class object {};

using object_ptr = std::unique_ptr<object>;

class integer : public object {
  int m_value;

public:
  explicit integer(int value);
  int value() const;
};

class boolean : public object {
  bool m_value;

public:
  explicit boolean(bool value);
  bool value() const;
};

} // namespace minsk::runtime

#endif // MINSK_OBJECT_HPP
