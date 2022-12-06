use std::collections::HashSet;

pub fn solve() {
    let input = include_str!("../inputs/06.txt");
    println!(
        "day 6-1: {}",
        find_start(input, 4).expect("unable to find start")
    );
    println!(
        "day 6-2: {}",
        find_start(input, 14).expect("unable to find start")
    );
}

fn find_start(message: &str, size: usize) -> Option<usize> {
    (0..message.len() - size)
        .find(|&i| {
            message
                .chars()
                .skip(i)
                .take(size)
                .collect::<HashSet<_>>()
                .len()
                == size
        })
        .map(|i| i + size)
}
