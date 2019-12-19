use std::{fs, io, path::Path};

/// An instance of an Intcode program.
pub struct Program {
    /// The program's memory.
    memory: Vec<usize>,
    /// The current address the program is on.
    address: usize,
}

impl Program {
    /// Load an Intcode program.
    pub fn new(src: &[usize]) -> Self {
        Self {
            memory: src.to_owned(),
            address: 0,
        }
    }

    /// Load an Intcode program from a source file.
    pub fn from_src(path: impl AsRef<Path>) -> Result<Self, io::Error> {
        let input = fs::read_to_string(path.as_ref())?;

        Ok(Self::new(&parse_src(&input)))
    }

    /// Run an Intcode program and produce its result.
    pub fn run(&mut self) -> usize {
        let opcode = self.memory[self.address];

        if opcode == 99 {
            return self.memory[0];
        }

        let arg_1 = self.memory[self.memory[self.address + 1]];
        let arg_2 = self.memory[self.memory[self.address + 2]];
        let out_address = self.memory[self.address + 3];

        match opcode {
            1 => self.memory[out_address] = arg_1 + arg_2,
            2 => self.memory[out_address] = arg_1 * arg_2,
            _ => unreachable!(),
        }

        self.address += 4;
        self.run()
    }
}

/// Parse an Intcode source file.
pub fn parse_src(input: &str) -> Vec<usize> {
    input
        .trim_end()
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_program() {
        let mut program = Program::new(&[1, 0, 0, 0, 99]);

        assert_eq!(program.run(), 2);
    }
}
