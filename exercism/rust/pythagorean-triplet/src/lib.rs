use std::collections::HashSet;

pub fn find(n: u32) -> HashSet<[u32; 3]> {
  let mut triples = HashSet::new();

  for a in 1..=n {
    for b in 1..=n - a {
      let c = n - a - b;

      if a * a + b * b == c * c && a < b {
        triples.insert([a, b, c]);
      }
    }
  }

  triples
}
