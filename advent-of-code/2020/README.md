# Advent of Code 2020

These are my solutions for the [Advent of Code
2020](https://adventofcode.com/2020).

The language I have chosen to implement my solutions in this year is
[Rust](https://rust-lang.org/), with an additional attempt to write the
solutions in modern C++ and Haskell when I fancy.

To run a Rust solution, enter its directory and invoke the program with Cargo.
For example:

```sh
$ cd rust/day01
$ cargo run
```

To run a C++ solution, enter the `cpp` directory, build the solutions, and then
run its binary. For example:

```sh
$ cd cpp
$ meson build
$ ninja -C build
$ build/day01
```

To run a Haskell solution, enter its directory, build the program with `ghc`,
and then run it. For example:

```sh
$ cd haskell
$ ghc --make -O2 day01.hs
$ ./day01
```

Alternatively, `runhaskell` can be used to directly run a program interpreted
(beware: worse performance).
