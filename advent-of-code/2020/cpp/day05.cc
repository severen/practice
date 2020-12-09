#include <iostream>
#include <fstream>
#include <string>
#include <vector>

unsigned int parse_id(std::string str) {
  unsigned int id = 0;
  for (auto chr : str) {
    id <<= 1;
    id |= (chr == 'B' || chr == 'R');
  }

  return id;
}

std::vector<unsigned int> parse_input(std::ifstream &input) {
  std::vector<unsigned int> ids;

  std::string line;
  while (getline(input, line)) {
    ids.push_back(parse_id(line));
  }

  return ids;
}

void part1(std::vector<unsigned int> &ids) {
  unsigned int max_id = *std::max_element(ids.begin(), ids.end());
  std::cout << "Part 1: " << max_id << "\n";
}

int main() {
  std::ifstream input("day05.txt");
  auto ids = parse_input(input);

  part1(ids);

  return 0;
}
