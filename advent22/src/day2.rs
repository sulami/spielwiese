use std::fs;

#[allow(clippy::identity_op)]
pub fn solve() {
    let input = fs::read_to_string("inputs/02.txt").expect("failed to read input");

    let part1 = |plays: &str| -> i32 {
        match plays {
            "A X" => 1 + 3,
            "A Y" => 2 + 6,
            "A Z" => 3 + 0,
            "B X" => 1 + 0,
            "B Y" => 2 + 3,
            "B Z" => 3 + 6,
            "C X" => 1 + 6,
            "C Y" => 2 + 0,
            "C Z" => 3 + 3,
            _ => panic!("oh no"),
        }
    };

    let part2 = |plays: &str| -> i32 {
        match plays {
            "A X" => 3 + 0,
            "A Y" => 1 + 3,
            "A Z" => 2 + 6,
            "B X" => 1 + 0,
            "B Y" => 2 + 3,
            "B Z" => 3 + 6,
            "C X" => 2 + 0,
            "C Y" => 3 + 3,
            "C Z" => 1 + 6,
            _ => panic!("oh no"),
        }
    };

    let calculate = |i: &str, strategy: fn(&str) -> i32| -> i32 { i.lines().map(strategy).sum() };

    println!("day 2-1: {}", calculate(&input, part1));
    println!("day 2-2: {}", calculate(&input, part2));
}
