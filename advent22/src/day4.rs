use std::fs;
use std::str::FromStr;

pub fn solve() {
    let input = fs::read_to_string("inputs/04.txt").expect("failed to read input");
    let pairs: Vec<Pair> = input.lines().map(|l| l.parse().unwrap()).collect();
    println!(
        "day 4-1: {}",
        pairs.iter().filter(|p| fully_contains(p)).count()
    );
    println!("day 4-2: {}", pairs.iter().filter(|p| overlaps(p)).count());
}

#[derive(Debug)]
struct Pair {
    first: Range,
    second: Range,
}

impl FromStr for Pair {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let [first, second] = s.split(',').collect::<Vec<&str>>()[..2] {
            Ok(Self {
                first: first.parse()?,
                second: second.parse()?,
            })
        } else {
            Err("invalid pair")
        }
    }
}

#[derive(Debug)]
struct Range {
    start: u32,
    end: u32,
}

impl FromStr for Range {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let [start, end] = s.split('-').collect::<Vec<&str>>()[..2] {
            Ok(Range {
                start: start.parse().unwrap(),
                end: end.parse().unwrap(),
            })
        } else {
            Err("invalid range")
        }
    }
}

fn fully_contains(pair: &Pair) -> bool {
    let ax = pair.first.start;
    let ay = pair.first.end;
    let bx = pair.second.start;
    let by = pair.second.end;
    (ax <= bx && ay >= by) || (bx <= ax && by >= ay)
}

fn overlaps(pair: &Pair) -> bool {
    let ax = pair.first.start;
    let ay = pair.first.end;
    let bx = pair.second.start;
    let by = pair.second.end;
    (bx >= ax && bx <= ay)
        || (by >= ax && by <= ay)
        || (ax >= bx && ax <= by)
        || (ay >= bx && ay <= by)
}
