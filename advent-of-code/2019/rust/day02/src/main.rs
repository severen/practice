use std::fs;

use intcode::Program;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let src: Vec<usize> = fs::read_to_string("input.txt")?
        .split(',')
        .map(str::trim)
        .map(|x| x.parse().unwrap())
        .collect();

    part1(&src);
    part2(&src);

    Ok(())
}

fn part1(src: &Vec<usize>) {
    let mut src = src.clone();
    src[1] = 12;
    src[2] = 2;

    let res = Program::new(&src).run();
    println!("Part 1: {}", res);
}

fn part2(src: &Vec<usize>) {
    for noun in 0..=99 {
        for verb in 0..=99 {
            let mut src = src.clone();
            src[1] = noun;
            src[2] = verb;

            let res = Program::new(&src).run();

            if res == 19_690_720 {
                println!("Part 2: {}", 100 * noun + verb);

                break;
            }
        }
    }
}
