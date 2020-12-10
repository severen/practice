project('practice', 'c', version: '0.1.0',
  default_options: [
    'c_std=c18',
    'c_args=-Wextra -Wpedantic -Wconversion',
    'b_pie=true',
])

cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required : false)

executable('nth_prime', 'nth_prime.c', dependencies : m_dep)
executable('fibonacci', 'fibonacci.c', dependencies : m_dep)
