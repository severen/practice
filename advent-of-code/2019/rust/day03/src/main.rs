use std::{collections::HashSet, fs};

use common::Point;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

type Wire = Vec<Segment>;
type Segment = (Direction, i32);

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let wires: Vec<Wire> = input
        .trim_end()
        .split('\n')
        .map(|instruction| instruction
             .split(',')
             .map(parse_instruction)
             .collect()
        )
        .collect();

    part1(&wires);

    Ok(())
}

fn part1(wires: &Vec<Wire>) {
    let points_1 = get_traversed_points(&wires[0]);
    let points_2 = get_traversed_points(&wires[1]);

    let closest_intersection = points_1
        .intersection(&points_2)
        .map(|point| point.distance(Point::new(0, 0)))
        .min()
        .unwrap();

    println!("Part 1: {}", closest_intersection);
}

/// Parse an instruction from a string into a direction and length.
fn parse_instruction(x: &str) -> (Direction, i32) {
    let direction = match x.chars().next().unwrap() {
        'U' => Direction::Up,
        'D' => Direction::Down,
        'L' => Direction::Left,
        'R' => Direction::Right,
        _ => unreachable!(),
    };
    let length = x[1..].parse::<i32>().unwrap();

    (direction, length)
}

/// Create the set of all points traversed by a wire.
fn get_traversed_points(wire: &Vec<Segment>) -> HashSet<Point> {
    // The current position in the grid.
    let mut cursor = Point::new(0, 0);
    // Set of all the traversed points.
    let mut points = HashSet::new();

    for segment in wire {
        match segment.0 {
            Direction::Up => {
                let end = cursor + Point::new(0, segment.1);

                for i in 1..=segment.1 {
                    let point = cursor + Point::new(0, i);
                    points.insert(point);
                }

                cursor = end;
            }
            Direction::Down => {
                let end = cursor - Point::new(0, segment.1);

                for i in 1..=segment.1 {
                    let point = cursor - Point::new(0, i);
                    points.insert(point);
                }

                cursor = end;
            }
            Direction::Left => {
                let end = cursor - Point::new(segment.1, 0);

                for i in 1..=segment.1 {
                    let point = cursor - Point::new(i, 0);
                    points.insert(point);
                }

                cursor = end;
            }
            Direction::Right => {
                let end = cursor + Point::new(segment.1, 0);

                for i in 1..=segment.1 {
                    let point = cursor + Point::new(i, 0);
                    points.insert(point);
                }

                cursor = end;
            }
        }
    }

    points
}
