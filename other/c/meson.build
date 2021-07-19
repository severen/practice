project('practice', 'c',
  version: '0.1.0',
  default_options: [
    'c_std=c17',
    'c_args=-Wall -Wextra -Wpedantic -Wconversion',
    'b_pie=true',
])

cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required: false)

common = static_library('common', 'common.c', dependencies: m_dep)
executable('fib', 'fib.c', dependencies: m_dep, link_with: common)
executable('nth_prime', 'nth_prime.c', dependencies: m_dep, link_with: common)

subdir('huffman')
