use std::{result, error, fs, collections::HashMap};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

type Passport<'a> = HashMap<&'a str, &'a str>;

const REQUIRED_KEYS: [&'static str; 7] =
  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let passports: Vec<Passport> = input
    // TODO: Figure out a platform-agnostic way to split on an empty line.
    .split("\n\n")
    .map(parse_passport)
    .collect();

  part1(&passports);

  Ok(())
}

fn part1(passports: &[Passport]) {
  let mut count = 0;

  for passport in passports {
    if has_required_keys(passport) {
      count += 1;
    }
  }

  println!("Part 1: {}", count);
}

fn parse_passport(text: &str) -> Passport {
  let fields = text
    .split_whitespace()
    .flat_map(|f| f.split(':'));
  let keys = fields.clone().step_by(2);
  let values = fields.skip(1).step_by(2);

  keys.zip(values).collect()
}

fn has_required_keys(passport: &Passport) -> bool {
  for key in &REQUIRED_KEYS {
    if !passport.contains_key(key) {
      return false;
    }
  }

  true
}
