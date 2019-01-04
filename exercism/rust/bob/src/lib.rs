fn is_yelling(message: &str) -> bool {
    // We check for an alphabetic character so that a string like ":)" is not
    // accepted.
    message.to_uppercase() == message && message.contains(char::is_alphabetic)
}

pub fn reply(message: &str) -> &str {
    match message.trim() {
        m if is_yelling(m) && m.ends_with('?') => "Calm down, I know what I'm doing!",
        m if is_yelling(m) => "Whoa, chill out!",
        m if m.ends_with('?') => "Sure.",
        m if m.is_empty() => "Fine. Be that way!",
        _ => "Whatever.",
    }
}
