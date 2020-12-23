#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>

std::vector<int> parse_input(std::ifstream& input) {
  std::vector<int> xs;

  std::string line;
  while (getline(input, line)) {
    xs.push_back(std::stoi(line));
  }

  return xs;
}

void part1(const std::vector<int>& xs, const int target) {
  std::set<int> seen;

  for (auto x : xs) {
    auto y = target - x;

    if (seen.contains(y)) {
      std::cout << "Part 1: " << x * y << "\n";
      return;
    }

    seen.insert(x);
  }
}

void part2(const std::vector<int>& xs, const int target) {
  for (auto i = xs.begin(); i != xs.end(); ++i) {
    std::set<int> seen;

    for (auto j = i + 1; j != xs.end(); ++j) {
      auto x = *i;
      auto y = *j;
      auto z = target - x - y;

      if (seen.contains(z)) {
        std::cout << "Part 2: " << x * y * z << "\n";
        return;
      }

      seen.insert(y);
    }
  }
}

int main() {
  std::ifstream input("day01.txt");
  auto xs = parse_input(input);
  
  const int target = 2020;
  
  part1(xs, target);
  part2(xs, target);

  return 0;
}
