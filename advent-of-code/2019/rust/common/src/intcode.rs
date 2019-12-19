/// An instance of an Intcode program.
pub struct Program {
    /// The program's memory.
    memory: Vec<usize>,
    /// The current address the program is on.
    address: usize,
}

impl Program {
    pub fn new(src: &Vec<usize>) -> Self {
        Self {
            memory: src.clone(),
            address: 0,
        }
    }

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_program() {
        let mut program = Program::new(&vec![1, 0, 0, 0, 99]);

        assert_eq!(program.run(), 2);
    }
}
