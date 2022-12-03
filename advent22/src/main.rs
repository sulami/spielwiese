use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    day1();
    day2();
    day3();
}

fn day1() {
    let input = fs::read_to_string("inputs/01.txt").expect("failed to read input file");
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

#[allow(clippy::identity_op)]
fn day2() {
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

fn split_in_half<'a>(rs: &'a str) -> (&'a str, &'a str) {
    let l = rs.len() / 2;
    (&rs[..l], &rs[l..])
}

fn day3() {
    let input = fs::read_to_string("inputs/03.txt").expect("failed to read input");
    let rucksacks: Vec<&str> = input.lines().collect();
    let duplicates: Vec<char> = rucksacks
        .iter()
        .map(|&s| -> char {
            let (a, b) = split_in_half(s);
            let sa: HashSet<char> = a.chars().collect();
            let sb: HashSet<char> = b.chars().collect();
            let both: Vec<&char> = sa.intersection(&sb).collect();
            **both.first().expect("no misplaced item found")
        })
        .collect();
    let priorities_map: HashMap<char, u32> = ('a'..='z').chain('A'..='Z').zip(1..).collect();
    let get_priority =
        |c: &char| -> u32 { *priorities_map.get(&c).expect("unable to find priority") };
    println!(
        "day 3-1: {}",
        duplicates.iter().map(get_priority).sum::<u32>()
    );

    let badges: Vec<char> = rucksacks
        .chunks(3)
        .map(|chunk| -> char {
            if let [a, b, c] = chunk {
                let sa: HashSet<char> = a.chars().collect();
                let sb: HashSet<char> = b.chars().collect();
                let sc: HashSet<char> = c.chars().collect();
                sa.intersection(&sb)
                    .map(|x| x.to_owned())
                    .collect::<HashSet<char>>()
                    .intersection(&sc)
                    .map(|x| x.to_owned())
                    .collect::<Vec<char>>()
                    .first()
                    .expect("no common badge item found")
                    .to_owned()
            } else {
                panic!("chunking rucksacks failed");
            }
        })
        .collect();

    println!("day 3-2: {}", badges.iter().map(get_priority).sum::<u32>());
}
