#ifndef MINSK_OBJECT_HPP
#define MINSK_OBJECT_HPP

#include <memory>
#include <ostream>

namespace minsk::runtime {

enum class object_kind {
  integer,
  boolean,
};

class object {
public:
  virtual object_kind kind() const = 0;
  virtual std::ostream &print(std::ostream &os) const = 0;
  virtual ~object() = default;
};

using object_ptr = std::unique_ptr<object>;

class integer final : public object {
  int m_value;

public:
  explicit integer(int value);
  object_kind kind() const override;
  std::ostream &print(std::ostream &os) const override;
  int value() const;
};

class boolean final : public object {
  bool m_value;

public:
  explicit boolean(bool value);
  object_kind kind() const override;
  std::ostream &print(std::ostream &os) const override;
  bool value() const;
};

std::unique_ptr<object> copy_object_ptr(const object *ptr);

} // namespace minsk::runtime

#endif // MINSK_OBJECT_HPP
