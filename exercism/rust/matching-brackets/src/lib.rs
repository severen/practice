pub fn brackets_are_balanced(string: &str) -> bool {
    let mut stack = Vec::new();

    for c in string.chars() {
        match c {
            '(' | '[' | '{' => stack.push(c),
            ')' | ']' | '}' => {
                let top = stack.pop();

                match c {
                    ')' => if top != Some('(') {
                        return false;
                    },
                    ']' => if top != Some('[') {
                        return false;
                    },
                    '}' => if top != Some('{') {
                        return false;
                    },
                    _ => unreachable!(),
                }
            },
            _ => continue,
        }
    }

    // If the stack is not empty, there are unbalanced brackets.
    stack.is_empty()
}
