use std::{
    io::Read,
    fs::File,
    collections::HashMap,
};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let mut input = String::new();
    File::open("input.txt")?.read_to_string(&mut input)?;

    part1(&input)?;
    part2(&input)?;

    Ok(())
}

fn part1(input: &str) -> Result<()> {
    let mut twos = 0;
    let mut threes = 0;

    for id in input.lines() {
        let mut frequencies = HashMap::new();

        for char in id.chars() {
            let frequency = frequencies.entry(char).or_insert(0);

            *frequency += 1;
        }

        if frequencies.iter().any(|(_, &frequency)| frequency == 2) {
            twos += 1;
        }

        if frequencies.iter().any(|(_, &frequency)| frequency == 3) {
            threes += 1;
        }
    }

    println!("Checksum: {}.", twos * threes);

    Ok(())
}

fn part2(input: &str) -> Result<()> {
    let ids: Vec<&str> = input.lines().collect();

    for i in 0..ids.len() {
        for j in i + 1..ids.len() {
            let (id1, id2) = (ids[i], ids[j]);

            if differs_by_one(id1, id2) {
                let common_chars: String = id1.chars().zip(id2.chars())
                    .filter(|&(c1, c2)| c1 == c2)
                    .map(|(c, _)| c)
                    .collect();

                println!("Common letters: {}.", common_chars);

                return Ok(());
            }
        }
    }

    Ok(())
}

/// Check whether the given IDs differ by *only* 1 letter.
fn differs_by_one(id1: &str, id2: &str) -> bool {
    let mut differences = 0;

    for (char1, char2) in id1.chars().zip(id2.chars()) {
        if char1 != char2 {
            differences += 1;

            if differences > 1 {
                return false
            }
        }
    }

    true
}
