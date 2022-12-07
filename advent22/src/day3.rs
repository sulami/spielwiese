use std::collections::{HashMap, HashSet};

pub fn solve() {
    let input = include_str!("../inputs/03.txt");
    let rucksacks: Vec<&str> = input.lines().collect();
    let duplicates: Vec<char> = rucksacks
        .iter()
        .map(|&s| -> char {
            let (a, b) = split_in_half(s);
            let sa: HashSet<char> = a.chars().collect();
            let sb: HashSet<char> = b.chars().collect();
            let both: Vec<char> = sa.intersection(&sb).copied().collect();
            both.first().expect("no misplaced item found").to_owned()
        })
        .collect();
    let priorities_map: HashMap<char, u32> = ('a'..='z').chain('A'..='Z').zip(1..).collect();
    let get_priority =
        |c: &char| -> &u32 { priorities_map.get(c).expect("unable to find priority") };
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
                    .copied()
                    .collect::<HashSet<char>>()
                    .intersection(&sc)
                    .copied()
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

fn split_in_half(rs: &str) -> (&str, &str) {
    let l = rs.len() / 2;
    (&rs[..l], &rs[l..])
}
