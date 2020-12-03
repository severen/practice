use std::{result, error, fs};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

type Map = Vec<Vec<char>>;

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let map: Map = input
    .lines()
    .map(|line| line.chars().collect())
    .collect();

  part1(&map);
  part2(&map);

  Ok(())
}

fn part1(map: &Map) {
  println!("Part 1: {}", count_trees(&map, (1, 3)));
}

fn part2(map: &Map) {
  let slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)];
  let result = slopes
    .iter()
    .fold(1, |acc, slope| acc * count_trees(map, *slope));

  println!("Part 2: {}", result);
}

fn count_trees(map: &Map, slope: (usize, usize)) -> usize {
  let rows = map.len();
  let cols = map[0].len();
  let (dy, dx) = slope;

  let mut count = 0;

  for i in 0..rows / dy {
    let square = map[i * dy][(i * dx) % cols];

    if square == '#' {
      count += 1;
    }
  }

  count
}
