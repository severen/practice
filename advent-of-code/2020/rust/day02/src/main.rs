use std::{result, error, fs};

use regex::Regex;
use once_cell::sync::Lazy;

type Result<T> = result::Result<T, Box<dyn error::Error>>;

struct Policy {
  min: i32,
  max: i32,
  chr: char,
}

static REGEX: Lazy<Regex> = Lazy::new(||
  Regex::new(r"^([0-9]+)-([0-9]+) ([A-Za-z]): ([A-Za-z]+)$").unwrap()
);

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?
    .lines()
    .map(parse_line)
    .collect::<Result<Vec<_>>>()?;

  part1(&input);
  part2(&input);

  Ok(())
}

fn parse_line(line: &str) -> Result<(String, Policy)> {
  let captures = REGEX.captures(line).ok_or("failed to parse input")?;

  let (min, max, chr) = (
    captures[1].parse()?,
    captures[2].parse()?,
    captures[3].chars().nth(0).unwrap(),
  );
  let password = &captures[4];

  Ok((password.to_string(), Policy { min, max, chr }))
}

fn part1(data: &[(String, Policy)]) {
  let mut count = 0;

  for (password, policy) in data {
    let mut matches = 0;

    for chr in password.chars() {
      if chr == policy.chr {
        matches += 1;
      }
    }

    if policy.min <= matches && matches <= policy.max {
      count += 1;
    }
  }

  println!("Part 1: {}", count);
}

fn part2(data: &[(String, Policy)]) {
  let mut count = 0;

  for (password, policy) in data {
    let mut valid = false;
    let (n, m) = (policy.min, policy.max);

    for i in &[(n - 1), (m - 1)] {
      let chr = password.chars().nth(*i as usize).unwrap();

      if chr == policy.chr {
        valid = !valid;
      }
    }

    if valid {
      count += 1;
    }
  }

  println!("Part 2: {}", count);
}
