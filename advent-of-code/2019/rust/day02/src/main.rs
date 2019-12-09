use std::fs;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;

    part1(&input)?;
    part2(&input)?;

    Ok(())
}

fn part1(input: &str) -> Result<()> {
    let mut program: Vec<usize> = input
        .split(',')
        .map(str::trim)
        .map(|x| x.parse().unwrap())
        .collect();

    program[1] = 12;
    program[2] = 2;

    run(&mut program, 0);

    println!("Part 1: {}", program[0]);

    Ok(())
}

fn part2(input: &str) -> Result<()> {
    for noun in 0..=99 {
        for verb in 0..=99 {
            let mut program: Vec<usize> = input
                .split(',')
                .map(str::trim)
                .map(|x| x.parse().unwrap())
                .collect();

            program[1] = noun;
            program[2] = verb;

            run(&mut program, 0);

            if program[0] == 19_690_720 {
                println!("Part 2: {}", 100 * noun + verb);

                break;
            }
        }
    }

    Ok(())
}

fn run(program: &mut Vec<usize>, pos: usize) {
    match program[pos] {
        1 => {
            let augend_pos = program[pos + 1];
            let addend_pos = program[pos + 2];
            let out_pos = program[pos + 3];

            program[out_pos] = program[augend_pos] + program[addend_pos];
        },
        2 => {
            let multiplier_pos = program[pos + 1];
            let multiplicand_pos = program[pos + 2];
            let out_pos = program[pos + 3];

            program[out_pos] = program[multiplier_pos] * program[multiplicand_pos];
        },
        99 => {
            return;
        },
        _ => unreachable!(),
    }

    run(program, pos + 4);
}