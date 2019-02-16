fn isqrt(n: u32) -> u32 {
    (n as f64).sqrt() as u32
}

fn is_prime(n: u32) -> bool {
    if n < 2 {
        false
    } else {
        (2..=isqrt(n)).all(|m| n % m != 0)
    }
}

pub fn nth(n: u32) -> u32 {
    (2..).filter(|x| is_prime(*x)).nth(n as usize).unwrap()
}
