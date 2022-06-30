#include <boost/functional/hash.hpp>
#include <iostream>
#include <ctime>
#include <unordered_map>

constexpr int N_INTS = 2;
constexpr int N_KEYS = 1e7;

int main() {
  std::cout << "Start C++ STL benchmark:\n";

  typedef std::array<int, N_INTS> KeyType;
  std::unordered_map<std::array<int, N_INTS>, double, boost::hash<KeyType>> h;

  h.reserve(N_KEYS * 2);
  const double t0 = std::clock();

  KeyType key;
  for (int i = 1; i <= N_KEYS; i++) {
    for (int j = 1; j <= N_INTS; j++) {
      key[j - 1] = i + j;
    }
    h[key] = i * 0.5;
  }
  const double t1 = std::clock();

  double val;
  for (int i = 1; i <= N_KEYS; i++) {
    for (int j = 1; j <= N_INTS; j++) {
      key[j - 1] = i + j;
    }
    val = h[key];
  }
  const double t2 = std::clock();

  h.clear();
  const double t3 = std::clock();

  std::cout << "Time to assemble / get / clear:"
      << "       " << (t1 - t0) / CLOCKS_PER_SEC
      << "       " << (t2 - t1) / CLOCKS_PER_SEC
      << "       " << (t3 - t2) / CLOCKS_PER_SEC
      << "\n";

  return 0;
}
