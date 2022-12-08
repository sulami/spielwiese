use std::str::FromStr;

pub fn solve() {
    let input = include_str!("../inputs/08.txt");
    let trees: Map = input.parse().expect("invalid map");
    let part1 = (0..trees.len()).filter(|&idx| trees.visible(idx)).count();
    println!("day 7-1: {}", part1);
    let part2 = (0..trees.len())
        .map(|idx| trees.scenic_score(idx))
        .max()
        .unwrap();
    println!("day 7-2: {}", part2);
}

#[derive(Debug)]
struct Map {
    size: usize,
    inner: Vec<u8>,
}

impl FromStr for Map {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut size = 0;
        let inner: Vec<u8> = s
            .lines()
            .flat_map(|l| -> Vec<u8> {
                size += 1;
                l.chars()
                    .map(|c| c.to_digit(10).expect("invalid tree") as u8)
                    .collect()
            })
            .collect();

        Ok(Self { inner, size })
    }
}

impl Map {
    /// Returns the total size of the inner tree vec.
    fn len(&self) -> usize {
        self.size * self.size
    }

    /// For a given (x, y) tree coordinate, returns the index.
    fn idx(&self, x: usize, y: usize) -> usize {
        y * self.size + x
    }

    /// For a given index, returns the tree (x, y) coordinates.
    fn coords(&self, idx: usize) -> (usize, usize) {
        (idx.rem_euclid(self.size), idx / self.size)
    }

    /// Returns true if the tree at idx is visible from the outside.
    fn visible(&self, idx: usize) -> bool {
        let (x, y) = self.coords(idx);
        // On the edge?
        if x == 0 || y == 0 || x == self.size - 1 || y == self.size - 1 {
            return true;
        }
        let this_tree = self.inner[idx];
        // Same row - left
        if self.inner[self.idx(0, y)..idx]
            .iter()
            .all(|&t| t < this_tree)
        {
            return true;
        }
        // Same row - right
        if self.inner[idx + 1..self.idx(self.size, y)]
            .iter()
            .all(|&t| t < this_tree)
        {
            return true;
        }
        // Same column - top
        if self
            .inner
            .iter()
            .skip(x)
            .step_by(self.size)
            .take(y)
            .all(|&t| t < this_tree)
        {
            return true;
        }
        // Same column - bottom
        if self
            .inner
            .iter()
            .skip(x)
            .step_by(self.size)
            .skip(y + 1)
            .all(|&t| t < this_tree)
        {
            return true;
        }
        false
    }

    /// For a given index, returns that tree's scenic score.
    fn scenic_score(&self, idx: usize) -> u32 {
        let (x, y) = self.coords(idx);
        let this_tree = self.inner[idx];

        let view_distance = |trees: &mut dyn Iterator<Item = &u8>| -> u32 {
            let mut all = 0;
            let view_until = trees
                // NB This is a slightly clever trick to get the total
                // size of the iterator before limiting, without
                // having to collect items or iterate twice in some
                // way.
                .inspect(|_| all += 1)
                .take_while(|&t| *t < this_tree)
                .count();
            // Account for +1 unless we can see until the edge.
            all.min(view_until + 1) as u32
        };

        let left = view_distance(&mut self.inner[self.idx(0, y)..idx].iter().rev());
        let right = view_distance(&mut self.inner[idx + 1..self.idx(self.size, y)].iter());
        let up = view_distance(&mut self.inner.iter().skip(x).step_by(self.size).take(y).rev());
        let down = view_distance(&mut self.inner.iter().skip(x).step_by(self.size).skip(y + 1));
        left * right * up * down
    }
}
