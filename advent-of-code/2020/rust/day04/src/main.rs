use std::{result, error, fs, collections::HashSet};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

const REQUIRED_FIELDS: [&'static str; 7] =
  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let passports: Vec<String> = input
    .split("\n\n")
    .map(|line| line.replace('\n', " "))
    .collect();

  part1(&passports);

  Ok(())
}

fn part1(passports: &[String]) {
  let mut count = 0;

  for passport in passports {
    if is_valid(passport) {
      count += 1;
    }
  }

  println!("Part 1: {}", count);
}

fn is_valid(passport: &str) -> bool {
  let present_fields: HashSet<&str> = passport
    .split_whitespace()
    .map(|f| f.split(':').nth(0).unwrap())
    .collect();

  for field in &REQUIRED_FIELDS {
    if !present_fields.contains(field) {
      return false;
    }
  }

  true
}
