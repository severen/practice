use std::fs;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

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
        .filter(has_adjacent_digits)
        .filter(has_nondecreasing_digits)
        .collect::<Vec<i32>>()
        .len();

    println!("Part 1: {}", possibilities);
}

/// Convert a number into a vector of its digits.
fn digits(x: i32) -> Vec<i32> {
    let mut digits = Vec::new();
    let mut x = x;

    while x > 9 {
        digits.push(x % 10);
        x = x / 10;
    }

    digits.push(x);
    digits.reverse();

    digits
}

/// Determine if a number has adjacent digits.
fn has_adjacent_digits(x: &i32) -> bool {
    for window in digits(*x).windows(2) {
        if window[0] == window[1] {
            return true
        }
    }

    false
}

/// Determine if a number has nondecreasing digits.
fn has_nondecreasing_digits(x: &i32) -> bool {
    for window in digits(*x).windows(2) {
        if window[1] < window[0] {
            return false
        }
    }

    true
}
