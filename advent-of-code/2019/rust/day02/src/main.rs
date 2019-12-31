use std::fs;

use common::intcode::{self, Program};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let src = intcode::parse_src(&input);

    part1(&src)?;
    part2(&src)?;

    Ok(())
}

fn part1(src: &[usize]) -> Result<()> {
    let mut src = src.to_owned();
    src[1] = 12;
    src[2] = 2;

    let res = Program::new(&src).run()?;
    println!("Part 1: {}", res);

    Ok(())
}

fn part2(src: &[usize]) -> Result<()> {
    for noun in 0..=99 {
        for verb in 0..=99 {
            let mut src = src.to_owned();
            src[1] = noun;
            src[2] = verb;

            let res = Program::new(&src).run()?;

            if res == 19_690_720 {
                println!("Part 2: {}", 100 * noun + verb);

                break;
            }
        }
    }

    Ok(())
}
