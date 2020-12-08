use std::{result, error, fs};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let ids: Vec<_> = input.lines().map(parse_id).collect();

  part1(&ids);
  part2(&ids);

  Ok(())
}

fn part1(ids: &[u16]) {
  let max_id = ids.iter().max().expect("Input is empty.");
  println!("Part 1: {}", max_id);
}

fn part2(ids: &[u16]) {
  let mut ids = ids.to_vec();
  ids.sort_unstable();

  let id = ids
    .windows(2)
    .find(|window| window[0] + 1 != window[1])
    .map(|window| window[0] + 1)
    .unwrap();
  println!("Part 2: {}", id);
}

fn parse_id(location: &str) -> u16 {
  // This may look like magic, but all it does is convert a string of
  // partitions into a 16-bit integer. This makes sense because the input is
  // just a binary encoding in which, for the upper 7 bits, 1 corresponds to B
  // and 0 to F, and for the lower 3 bits, 1 corresponds to R and 0 to L.
  // Hence, the process of finding the unique seat ID by computing
  // row * 8 + col is equivalent to taking the binary value of the upper 7
  // bits, shifting them left, and then adding them to the lower 3 bits, which
  // leaves you with what you started with in the binary representation.
  location.chars().fold(0, |id, half| {
    (id << 1) | matches!(half, 'B' | 'R') as u16
  })
}
