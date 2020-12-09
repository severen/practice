use std::{result, error, fs, collections::HashMap};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

type Passport<'a> = HashMap<&'a str, &'a str>;

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let passports: Vec<Passport> = input
    // TODO: Figure out a platform-agnostic way to split on an empty line.
    .split_terminator("\n\n")
    .map(parse_passport)
    .collect();

  part1(&passports);
  part2(&passports);

  Ok(())
}

fn part1(passports: &[Passport]) {
  let count = passports.iter().fold(0, |acc, passport| {
    if has_required_keys(passport) {
      acc + 1
    } else {
      acc
    }
  });

  println!("Part 1: {}", count);
}

fn part2(passports: &[Passport]) {
  let count = passports.iter().fold(0, |acc, passport| {
    if is_valid(passport) {
      acc + 1
    } else {
      acc
    }
  });

  println!("Part 2: {}", count);
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
  const REQUIRED_KEYS: [&str; 7] =
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

  REQUIRED_KEYS.iter().all(|key| passport.contains_key(key))
}

fn is_valid(passport: &Passport) -> bool {
  has_required_keys(passport) && passport.iter().all(is_field_valid)
}

fn is_field_valid(field: (&&str, &&str)) -> bool {
  const EYE_COLORS: [&str; 7] =
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

  let (&key, &value) = field;
  match key {
    "byr" => (1920..=2002).contains(&value.parse().unwrap_or(0)),
    "iyr" => (2010..=2020).contains(&value.parse().unwrap_or(0)),
    "eyr" => (2020..=2030).contains(&value.parse().unwrap_or(0)),
    "hgt" => {
      let len = value.len();
      let height = &value[0..len - 2].parse().unwrap_or(0);
      let unit = &value[len - 2..];

      match unit {
        "cm" => (150..=193).contains(height),
        "in" => (59..=76).contains(height),
        _ => false,
      }
    },
    "hcl" =>
      value.starts_with('#') &&
      value.len() == 7 &&
      value.chars().skip(1).all(|c| c.is_ascii_hexdigit()),
    "ecl" => EYE_COLORS.contains(&value),
    "pid" => value.len() == 9 && value.chars().all(|c| c.is_ascii_digit()),
    "cid" => true,
    _ => unreachable!(),
  }
}
