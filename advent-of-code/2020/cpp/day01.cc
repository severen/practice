#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>

std::vector<int> parse_input(std::ifstream &input) {
  std::vector<int> xs;

  std::string line;
  while (getline(input, line)) {
    xs.push_back(std::stoi(line));
  }

  return xs;
}

void part1(std::vector<int> &xs, int target) {
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

void part2(std::vector<int> &xs, int target) {
  for (std::size_t i = 0; i < xs.size(); i++) {
    std::set<int> seen;

    for (std::size_t j = i + 1; j < xs.size(); j++) {
      auto x = xs[i];
      auto y = xs[j];
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
  int target = 2020;

  std::ifstream input("day01.txt");
  auto xs = parse_input(input);

  part1(xs, target);
  part2(xs, target);

  return 0;
}
