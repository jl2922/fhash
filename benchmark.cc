#include <boost/functional/hash.hpp>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <unordered_map>
#include <vector>

#define N_INTS 14
#define N_KEYS 10000000

void benchmark() {
  typedef std::vector<int> KeyType;
  const std::clock_t start = std::clock();
  std::unordered_map<KeyType, double, boost::hash<KeyType>> h;
  h.reserve(N_KEYS * 2);
  KeyType key(N_INTS);
  for (int i = 1; i <= N_KEYS; i++) {
    for (int j = 1; j <= N_INTS; j++) {
      key[j - 1] = i + j;
    }
    h[key] = i * 0.5;
  }
  const std::clock_t finish = std::clock();
  printf("Time insert: %.3g s\n",
         static_cast<double>(finish - start) / CLOCKS_PER_SEC);
  h.clear();
}

int main() {
  typedef std::vector<int> KeyType;
  const std::clock_t start = std::clock();
  benchmark();
  const std::clock_t finish = std::clock();
  printf("Time finish: %.3g s\n",
         static_cast<double>(finish - start) / CLOCKS_PER_SEC);

  return 0;
}