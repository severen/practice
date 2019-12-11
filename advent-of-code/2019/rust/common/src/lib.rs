use std::{
    ops::{Add, Sub},
    convert::From,
};

pub mod intcode;

/// A point in 2-dimensional space.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Point {
    x: i32,
    y: i32,
}

impl Point {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    /// Calculate the Manhattan distance to another point.
    pub fn distance(&self, other: Point) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for Point {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl From<(i32, i32)> for Point {
    fn from(point: (i32, i32)) -> Self {
        Self { x: point.0, y: point.1 }
    }
}
