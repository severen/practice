use std::{
    fs::File,
    io::Read,
    collections::HashSet,
};

type Result<T> = std::result::Result<T, Box<std::error::Error>>;

fn main() -> Result<()> {
    let mut input = String::new();
    File::open("input.txt")?.read_to_string(&mut input)?;

    part1(&input)?;
    part2(&input)?;

    Ok(())
}

fn part1(input: &String) -> Result<()> {
    let frequency: i32 = input
        .lines()
        .map(|line| line.parse::<i32>().unwrap())
        .sum();

    println!("{}", frequency);

    Ok(()) 
}

fn part2(input: &String) -> Result<()> {
    let mut frequency = 0;
    let mut seen: HashSet<i32> = HashSet::new();
    seen.insert(frequency);

    let changes = input
        .lines()
        .map(|line| line.parse::<i32>().unwrap())
        .cycle();

    for change in changes {
        frequency += change;

        if seen.contains(&frequency) {
            break;
        } else {
            seen.insert(frequency);
        }
    }

    println!("{}", frequency);
    Ok(())
}
