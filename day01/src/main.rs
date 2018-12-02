use std::{
    io::{self, Read},
    collections::HashSet,
};

type Result<T> = std::result::Result<T, Box<std::error::Error>>;

fn main() -> Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    part1(&input)?;
    part2(&input)?;

    Ok(())
}

fn part1(input: &String) -> Result<()> {
    let mut frequency = 0;

    for line in input.lines() {
        let change: i32 = line.parse()?;
        frequency += change;
    }

    println!("{}", frequency);

    Ok(()) 
}

fn part2(input: &String) -> Result<()> {
    let mut frequency = 0;
    let mut seen: HashSet<i32> = HashSet::new();
    seen.insert(frequency);

    loop {
        for line in input.lines() {
            let change: i32 = line.parse()?;
            frequency += change;

            if seen.contains(&frequency) {
                println!("{}", frequency);
                return Ok(());
            }
            seen.insert(frequency);
        }
    }
}
