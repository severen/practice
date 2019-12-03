use std::fs;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;

    part1(&input)?;

    Ok(())
}

fn part1(input: &str) -> Result<()> {
    let required_fuel: i32 = input
        .lines()
        .map(|line| line.parse::<i32>().unwrap())
        .map(|mass| (mass / 3) - 2)
        .sum();

    println!("{}", required_fuel);

    Ok(())
}
