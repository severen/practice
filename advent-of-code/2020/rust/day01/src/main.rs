use std::{result, error, fs, collections::HashSet};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
  let target = 2020;
  let input: Vec<i32> = fs::read_to_string("input.txt")?
    .lines()
    .map(|line| line.parse().unwrap())
    .collect();

  part1(&input, target);
  part2(&input, target);

  Ok(())
}

// Part 1 boils down to the 'two sum' problem, which is solvable with a
// worst-case time complexity of O(n).
fn part1(xs: &[i32], target: i32) {
  let mut seen = HashSet::new();

  for x in xs {
    let y = target - x;

    if seen.contains(&y) {
      println!("Part 1: {}", x * y);
      return;
    }

    seen.insert(x);
  }
}

// Similarly to part 1, part 2 boils down to the 'three sum' problem, which is
// solvable with a worst-case time complexity of O(n^2).
fn part2(xs: &[i32], target: i32) {
  for i in 0..xs.len() {
    let mut seen = HashSet::new();

    for j in i + 1..xs.len() {
      let x = xs[i];
      let y = xs[j];
      let z = target - x - y;

      if seen.contains(&z) {
        println!("Part 2: {}", x * y * z);
        return;
      }

      seen.insert(y);
    }
  }
}
