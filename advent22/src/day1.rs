pub fn solve() {
    let input = include_str!("../inputs/01.txt");
    let count_calories = |elf: &str| -> u32 {
        elf.split_whitespace()
            .map(|n| n.parse::<u32>().expect("failed to parse calories"))
            .sum()
    };
    let mut elves: Vec<u32> = input.split("\n\n").map(count_calories).collect();
    elves.sort();
    println!(
        "day 1-1: {}\nday 1-2: {}",
        elves.last().unwrap(),
        elves.iter().rev().take(3).sum::<u32>()
    );
}
