fn isqrt(n: u32) -> u32 {
  (n as f64).sqrt() as u32
}

fn is_prime(n: u32) -> bool {
  if n < 2 || n > 2 && n % 2 == 0 {
    false
  } else if n == 2 {
    true
  } else {
    // We take advantage of the fact that the only prime numbers with a gap less
    // than 2 are 2 and 3, which are already covered conditionally.
    (3..=isqrt(n)).step_by(2).all(|m| n % m != 0)
  }
}

pub fn nth(n: u32) -> u32 {
  (2..).filter(|x| is_prime(*x)).nth(n as usize).unwrap()
}
