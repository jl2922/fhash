#include <boost/functional/hash.hpp>
#include <iostream>
#include <ctime>
#include <unordered_map>

constexpr int N_INTS = 2;
constexpr int N_KEYS = 10000000;

int main() {
  std::cout << "Start C++ STL benchmark:\n";

  typedef std::array<int, N_INTS> KeyType;
  std::unordered_map<std::array<int, N_INTS>, double, boost::hash<KeyType>> h;

  const double t0 = std::clock();
  h.reserve(N_KEYS * 2);
  KeyType key;
  for (int i = 1; i <= N_KEYS; i++) {
    for (int j = 1; j <= N_INTS; j++) {
      key[j - 1] = i + j;
    }
    h[key] = i * 0.5;
  }

  const double t1 = std::clock();
  h.clear();

  const double t2 = std::clock();
  std::cout << "Time to assemble: " << (t1 - t0) / CLOCKS_PER_SEC << "\n";
  std::cout << "Time to clear: " << (t2 - t1) / CLOCKS_PER_SEC << "\n";

  return 0;
}
