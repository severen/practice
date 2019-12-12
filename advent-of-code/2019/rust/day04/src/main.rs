use std::fs;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

/// A 6-digit number broken up into an array of its digits.
type Number = [i32; 6];

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let range: Vec<i32> = input
        .trim_end()
        .split('-')
        .map(|x| x.parse().unwrap())
        .collect();

    part1(&range);

    Ok(())
}

fn part1(range: &Vec<i32>) {
    let possibilities = (range[0]..=range[1])
        .map(digits)
        .filter(has_adjacent_digits)
        .filter(has_nondecreasing_digits)
        .collect::<Vec<Number>>()
        .len();

    println!("Part 1: {}", possibilities);
}

/// Convert a 6-digit number into an array of its digits.
fn digits(x: i32) -> Number {
    let mut digits: Number = [0; 6];
    let mut x = x;

    for i in (0..6).rev() {
        digits[i] = x % 10;
        x = x / 10;
    }

    digits
}

/// Determine if a 6-digit number has adjacent digits.
fn has_adjacent_digits(x: &Number) -> bool {
    for window in x.windows(2) {
        if window[0] == window[1] {
            return true
        }
    }

    false
}

/// Determine if a 6-digit number has nondecreasing digits.
fn has_nondecreasing_digits(x: &Number) -> bool {
    for window in x.windows(2) {
        if window[1] < window[0] {
            return false
        }
    }

    true
}
