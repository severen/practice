#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <utility>

using Map = std::vector<std::vector<char>>;

Map parse_input(std::ifstream &input) {
  Map map;

  std::string line;
  while (getline(input, line)) {
    map.push_back({ line.begin(), line.end() });
  }

  return map;
}

int count_trees(Map &map, std::pair<int, int> slope) {
  auto rows = map.size();
  auto cols = map[0].size();
  auto [ dy, dx ] = slope;

  int count = 0;

  for (int i = 0; i < rows / dy; i++) {
    auto square = map[i * dy][(i * dx) % cols];

    if (square == '#') {
      count++;
    }
  }

  return count;
}

void part1(Map &map) {
  std::cout << "Part 1: " << count_trees(map, { 1, 3 }) << "\n";
}

void part2(Map &map) {
  std::vector<std::pair<int, int>> slopes = {
    { 1, 1 }, { 1, 3 }, { 1, 5 }, { 1, 7 }, { 2, 1 }
  };

  int result = 1;
  for (auto slope : slopes) {
    result *= count_trees(map, slope);
  }

  std::cout << "Part 2: " << result << "\n";
}

int main() {
  std::ifstream input("day03.txt");
  auto map = parse_input(input);

  part1(map);
  part2(map);

  return 0;
}
