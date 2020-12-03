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

  Ok(())
}

fn part1(map: &Map) {
  let mut count = 0;

  let rows = map.len();
  let cols = map[0].len();

  for i in 0..rows {
    let square = map[i][(i * 3) % cols];

    if square == '#' {
      count += 1;
    }
  }

  println!("Part 1: {}", count);
}
