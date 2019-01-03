use std::{
    fs::File,
    io::Read,
    collections::HashMap,
};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let mut input = String::new();
    File::open("input.txt")?.read_to_string(&mut input)?;

    let mut twos = 0;
    let mut threes = 0;

    for line in input.lines() {
        let mut frequencies = HashMap::new();

        for char in line.chars() {
            let frequency = frequencies.entry(char).or_insert(0);

            *frequency += 1;
        }

        if frequencies.iter().any(|(_, &frequency)| frequency == 2) {
            twos += 1
        }

        if frequencies.iter().any(|(_, &frequency)| frequency == 3) {
            threes += 1
        }
    }

    println!("Checksum: {}.", twos * threes);

    Ok(())
}
