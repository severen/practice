use std::{result, error, fs};

type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
  let input = fs::read_to_string("input.txt")?;
  let lines: Vec<_> = input.lines().collect();

  let timestamp: i64 = lines[0].parse().unwrap();
  let ids: Vec<Option<i64>> = lines[1]
    .split(',')
    .map(|c| {
      if c != "x" {
        Some(c.parse().unwrap())
      } else {
        None
      }
    })
    .collect();

  part1(timestamp, &ids);
  part2(&ids);

  Ok(())
}

fn part1(timestamp: i64, ids: &[Option<i64>]) {
  let (wait, id) = ids
    .iter()
    .flatten()
    .map(|id| (id - timestamp % id, *id))
    .min()
    .unwrap();

  println!("Part 1: {}", wait * id);
}

fn part2(ids: &[Option<i64>]) {
  let residues: Vec<i64> = (0..)
    .zip(ids)
    .filter_map(|(i, id)| {
      if let Some(id) = id {
        Some((i, id))
      } else {
        None
      }
    })
    .map(|(i, id)| id - i)
    .collect();
  let moduli: Vec<i64> = ids.iter().flatten().cloned().collect();

  let answer = crt(&residues, &moduli).unwrap();
  println!("Part 2: {:?}", answer);
}

/// Find the solution to a modular system of equations with m residues and n
/// moduli according to the Chinese remainder theorem.
fn crt(residues: &[i64], moduli: &[i64]) -> Option<i64> {
  let prod: i64 = moduli.iter().product();

  let mut sum = 0;
  for (&residue, &modulus) in residues.iter().zip(moduli) {
    let p = prod / modulus;
    sum += residue * mod_inv(p, modulus)? * p;
  }

  Some(sum % prod)
}

/// Find the modular inverse of an integer x given a modulus n.
fn mod_inv(x: i64, n: i64) -> Option<i64> {
  let (g, (x, _)) = egcd(x, n);

  if g == 1 {
    Some((x % n + n) % n)
  } else {
    None
  }
}

/// Given two integers a and b, compute the coefficients of Bézout's identity
/// alongside the greatest common divisor using the extended Euclidean
/// algorithm.
fn egcd(a: i64, b: i64) -> (i64, (i64, i64)) {
  if a == 0 {
    (b, (0, 1))
  } else {
    let (g, (x, y)) = egcd(b % a, a);
    (g, (y - (b / a) * x, x))
  }
}
