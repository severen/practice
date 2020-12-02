use std::{result, error, fs};

use regex::Regex;
use lazy_static::lazy_static;

type Result<T> = result::Result<T, Box<dyn error::Error>>;

type Datum = ((i32, i32), (String, String));

// TODO: Replace this with Lazy when once_cell is stabilised.
lazy_static! {
  static ref REGEX: Regex =
    Regex::new(r"([0-9]+)-([0-9]+) ([A-Za-z]): ([A-Za-z]+)").unwrap();
}

fn main() -> Result<()> {
  let input: Vec<Datum> = fs::read_to_string("input.txt")?
    .lines()
    .map(parse_line)
    .collect();

  part1(&input);
  part2(&input);

  Ok(())
}

fn parse_line(line: &str) -> Datum {
  let captures = REGEX.captures(line).unwrap();

  let (min, max): (i32, i32) =
    (captures[1].parse().unwrap(), captures[2].parse().unwrap());
  let (letter, password) = (&captures[3], &captures[4]);

  ((min, max), (letter.to_string(), password.to_string()))
}

fn part1(data: &[Datum]) {
  let mut count = 0;

  for datum in data {
    let mut matches = 0;
    let ((min, max), (letter, password)) = datum;

    for chr in password.chars() {
      if chr.to_string() == *letter {
        matches += 1;
      }
    }

    if *min <= matches && matches <= *max {
      count += 1;
    }
  }

  println!("Part 1: {}", count);
}

fn part2(data: &[Datum]) {
  let mut count = 0;

  for datum in data {
    let mut valid = false;
    let ((n, m), (letter, password)) = datum;

    for i in &[(*n - 1), (*m - 1)] {
      let chr = password.chars().nth(*i as usize).unwrap();

      if chr.to_string() == *letter {
        valid = !valid;
      }
    }

    if valid {
      count += 1;
    }
  }

  println!("Part 2: {}", count);
}
