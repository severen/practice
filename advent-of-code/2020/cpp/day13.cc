#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <optional>
#include <ranges>

std::vector<int> parse_ids(const std::string& text) {
  std::vector<int> ids;

  std::string token;
  std::istringstream sstream(text);
  while (getline(sstream, token, ',')) {
    if (token != "x") {
      ids.push_back(stoi(token));
    }
  }

  return ids;
}

std::pair<int, std::vector<int>> parse_input(std::ifstream& input) {
  std::string line;
  std::vector<std::string> lines;
  while (getline(input, line)) {
    lines.push_back(line);
  }

  int timestamp = stoi(lines[0]);
  auto ids = parse_ids(lines[1]);

  return { timestamp, ids };
}

void part1(int timestamp, const std::vector<int>& ids) {
  auto v = ids | std::views::transform([timestamp](auto id) {
    return std::make_pair(id, id - timestamp % id);
  });

  auto [id, wait] = *std::min_element(v.begin(), v.end(), [](auto a, auto b) {
    return a.second < b.second;
  });

  std::cout << "Part 1: " << wait * id << '\n';
}

int main() {
  std::ifstream input("day13.txt");
  auto [ timestamp, ids ] = parse_input(input);

  part1(timestamp, ids);

  return 0;
}
