project('aoc-2020', 'cpp',
  version: '0.1.0',
  license: 'Unlicense',
  default_options: [
    'cpp_std=c++2a',
    'cpp_args=-Wextra -Wpedantic -Wconversion',
    'b_pie=true',
])

solutions = ['day01', 'day03', 'day05', 'day13']
foreach solution : solutions
  executable(solution, solution + '.cc')
endforeach
