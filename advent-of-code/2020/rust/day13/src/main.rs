use std::{result, error, fs};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let lines: Vec<_> = input.lines().collect();

  let timestamp: i32 = lines[0].parse().unwrap();
  let ids: Vec<i32> = lines[1]
    .split(',')
    .filter(|c| *c != "x")
    .map(|c| c.parse().unwrap())
    .collect();

  part1(timestamp, &ids);

  Ok(())
}

fn part1(timestamp: i32, buses: &[i32]) {
  let (wait, id) = buses
    .iter()
    .map(|x| (x - timestamp % x, *x))
    .min()
    .unwrap();

  println!("Part 1: {}", wait * id);
}
