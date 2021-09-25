use std::{result, error, fs, collections::HashSet};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let groups: Vec<_> = input
    .split_terminator("\n\n")
    .collect();

  part1(&groups);
  part2(&groups);

  Ok(())
}

fn part1(groups: &[&str]) {
  let count: usize = groups
    .iter()
    .map(|group| parse_group_v1(*group))
    .map(|answers| answers.len())
    .sum();

  println!("Part 1: {}", count);
}

fn part2(groups: &[&str]) {
  let count: usize = groups
    .iter()
    .map(|group| parse_group_v2(*group))
    .map(|answers| answers.len())
    .sum();

  println!("Part 2: {}", count);
}

fn parse_group_v1(group: &str) -> HashSet<char> {
  group.lines().flat_map(|line| line.chars()).collect()
}

fn parse_group_v2(group: &str) -> HashSet<char> {
  group
    .lines()
    .map(|line| line.chars().collect::<HashSet<_>>())
    .reduce(|a, b| &a & &b)
    .unwrap()
}
