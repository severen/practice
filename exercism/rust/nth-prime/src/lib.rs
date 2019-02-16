fn is_prime(n: u32) -> bool {
   // We add 1 to compensate for the conversion from f64 to u32, which truncates
   // the decimal point and therefore makes upper_bound^2 < n.
   let upper_bound = (n as f64).sqrt() as u32 + 1;
   (2..upper_bound).all(|m| n % m != 0)
}

pub fn nth(n: u32) -> u32 {
    (1..).filter(|x| is_prime(*x)).nth(n as usize + 1).unwrap()
}
