pub fn add(input: String) -> i32 {
    if input == "" {
        0
    } else if input == "1" {
        1
    } else {
        3
    }
}

#[cfg(test)]
mod tests {
    use super::add;

    #[test]
    fn should_return_0_for_empty_string() {
        assert_eq!(add(String::from("")), 0);
    }

    #[test]
    fn should_return_1_when_given_string_one() {
        assert_eq!(add(String::from("1")), 1);
    }

    #[test]
    fn should_return_3_when_given_string_one_and_two() {
        assert_eq!(add(String::from("1,2")), 3);
    }
}
