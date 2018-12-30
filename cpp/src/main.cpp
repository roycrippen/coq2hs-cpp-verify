#include <iostream>
#include <iomanip>
#include "../include/candidates.hpp"

int main(int argc, char *argv[])
{
  int a = 3;
  int b = 4;
  int c = 5;
  std::cout << "Is (" << a << ", " << b << ", " << c << ") a pythagorean triple: "
            << std::boolalpha << cn::is_pythagorean_triple(a, b, c) << "\n";
  c = 6;
  std::cout << "Is (" << a << ", " << b << ", " << c << ") a pythagorean triple: "
            << std::boolalpha << cn::is_pythagorean_triple(a, b, c) << "\n";

  std::string cs = "This is the test string...";
  std::cout << "input string: '" << cs << "'\n";
  auto encoded_vec = cn::encode(cs);
  std::cout << "encoded vector: " << cn::show(encoded_vec) << "\n";
  auto ds = cn::decode(encoded_vec);
  std::cout << "decoded string: '" << ds << "'\n";

  cs = "\32814\&3{H`G";
  std::cout << "input string: '" << cs << "'\n";
  encoded_vec = cn::encode(cs);
  std::cout << "encoded vector: " << cn::show(encoded_vec) << "\n";
  ds = cn::decode(encoded_vec);
  std::cout << "decoded string: '" << ds << "'\n";

  return 0;
}
