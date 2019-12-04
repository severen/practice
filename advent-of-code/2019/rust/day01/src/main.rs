use std::{fs, iter};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;

    part1(&input)?;
    part2(&input)?;

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

fn part2(input: &str) -> Result<()> {
    let calc = |mass: i32| ((mass / 3) - 2).max(0);

    let required_fuel: i32 = input
        .lines()
        .map(|line| line.parse::<i32>().unwrap())
        .map(calc)
        .flat_map(|mass| {
            iter::successors(Some(mass), |&x| {
                if x <= 0 {
                    None
                } else {
                    Some(calc(x))
                }
            })
        })
        .sum();

    println!("{}", required_fuel);

    Ok(())
}
