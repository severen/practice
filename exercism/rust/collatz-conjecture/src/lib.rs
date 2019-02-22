pub fn collatz(mut n: u64) -> Option<u64> {
    if n == 0 {
        return None;
    }

    for step in 0.. {
        if n == 1 {
            return Some(step);
        } else if n % 2 == 0 {
            n /= 2;
        } else {
            n = n * 3 + 1;
        }
    }

    None
}
