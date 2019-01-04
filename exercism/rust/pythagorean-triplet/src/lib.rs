pub fn find() -> Option<u32> {
    let s = 1000;

    for a in 1..=s {
        for b in 1..=s - a {
            let c = s - a - b;

            if a * a + b * b == c * c {
                return Some(a * b * c);
            }
        }
    }

    None
}
