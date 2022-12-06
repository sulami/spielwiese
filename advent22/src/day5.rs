use std::str::FromStr;

pub fn solve() {
    let input = include_str!("../inputs/05.txt");
    let stack_tops = |stacks: &Stacks| -> String {
        stacks
            .crates
            .iter()
            .map(|s| s.last().unwrap_or(&' '))
            .collect()
    };

    let (stacks, moves) = input.split_once("\n\n").expect("failed to split input");
    let mut stacks: Stacks = stacks.parse().expect("failed to parse stacks");

    moves
        .lines()
        .map(|m| m.parse::<Move>().expect("failed to parse move"))
        .try_for_each(|m| execute_move_9000(&mut stacks, &m))
        .expect("failed to execute moves");
    println!("day 5-1: {}", stack_tops(&stacks));

    let (stacks, moves) = input.split_once("\n\n").expect("failed to split input");
    let mut stacks: Stacks = stacks.parse().expect("failed to parse stacks");

    moves
        .lines()
        .map(|m| m.parse::<Move>().expect("failed to parse move"))
        .try_for_each(|m| execute_move_9001(&mut stacks, &m))
        .expect("failed to execute moves");
    println!("day 5-2: {}", stack_tops(&stacks));
}

fn execute_move_9000(stacks: &mut Stacks, m: &Move) -> Result<(), &'static str> {
    for _ in 0..m.num {
        let c = stacks.crates[m.from - 1]
            .pop()
            .ok_or("failed to execute move")?;
        stacks.crates[m.to - 1].push(c);
    }
    Ok(())
}

fn execute_move_9001(stacks: &mut Stacks, m: &Move) -> Result<(), &'static str> {
    let split_idx = stacks.crates[m.from - 1].len() - m.num;
    let cs = stacks.crates[m.from - 1].split_off(split_idx);
    stacks.crates[m.to - 1].extend(cs);
    Ok(())
}

#[derive(Clone, Debug, Default)]
struct Stacks {
    crates: Vec<Vec<char>>,
}

impl FromStr for Stacks {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let num = s
            .lines()
            .last()
            .ok_or("failed to get last stacks line")?
            .split_whitespace()
            .count();
        let mut stacks = Self {
            crates: vec![Vec::default(); num],
        };
        for line in s.lines().rev().skip(1) {
            for n in 0..num {
                // crate positions are 1 5 9 13 ...
                let c = line.chars().nth(1 + n * 4).ok_or("failed to get crate")?;
                if !c.is_whitespace() {
                    stacks.crates[n].push(c);
                }
            }
        }
        Ok(stacks)
    }
}

#[derive(Debug)]
struct Move {
    num: usize,
    from: usize,
    to: usize,
}

impl FromStr for Move {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut words = s.split_whitespace();
        let num = words
            .nth(1)
            .ok_or("failed to get move num")?
            .parse()
            .map_err(|_| "failed to parse move num")?;
        let from = words
            .nth(1)
            .ok_or("failed to get move from")?
            .parse()
            .map_err(|_| "failed to parse move from")?;
        let to = words
            .nth(1)
            .ok_or("failed to get move to")?
            .parse()
            .map_err(|_| "failed to parse move to")?;
        Ok(Self { num, from, to })
    }
}
