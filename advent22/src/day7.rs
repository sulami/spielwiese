use std::path::{Path, PathBuf};
use std::str::FromStr;

pub fn solve() {
    let input = include_str!("../inputs/07.txt");
    let entries: Vec<Entry> = input
        .split("$ ")
        .skip(1)
        .map(|e| e.parse().expect("failed to parse entry"))
        .collect();
    let fs_tree = build_fs_tree(&entries[1..]);
    let mut directories: Vec<&Node> = fs_tree.directories();

    let part1 = directories
        .iter()
        .copied()
        .filter_map(|n| {
            let s = n.get_size();
            if s <= 100_000 {
                Some(s)
            } else {
                None
            }
        })
        .sum::<u32>();
    println!("day 7-1: {}", part1);

    let required_space = 30_000_000 - (70_000_000 - directories[0].get_size());
    directories.sort_by_cached_key(|d| d.get_size());
    let part2 = directories
        .iter()
        .find(|d| d.get_size() >= required_space)
        .expect("no directory is large enough")
        .get_size();
    println!("day 7-2: {}", part2);
}

#[derive(Debug)]
struct Entry {
    command: Command,
    output: Vec<String>,
}

impl FromStr for Entry {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        let command: Command = lines.next().expect("failed to parse command").parse()?;
        let output: Vec<String> = lines.map(str::to_string).collect();
        Ok(Self { command, output })
    }
}

#[derive(Debug)]
enum Command {
    Cd { path: String },
    Ls,
}

impl FromStr for Command {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "ls" {
            return Ok(Self::Ls);
        }
        if let Some(("cd", path)) = s.split_once(' ') {
            Ok(Self::Cd {
                path: path.to_string(),
            })
        } else {
            Err("failed to parse command")
        }
    }
}

#[derive(Clone, Debug)]
enum Node {
    File { name: PathBuf, size: u32 },
    Dir { name: PathBuf, contents: Vec<Node> },
}

impl Node {
    fn get_name(&self) -> &PathBuf {
        match self {
            Self::File { name, .. } => name,
            Self::Dir { name, .. } => name,
        }
    }

    /// Given a root node, find a child node by following a path.
    fn find_node(&mut self, path: &Path) -> Option<&mut Node> {
        let mut current = self;
        for comp in path.components().skip(1) {
            if let Self::Dir { contents, .. } = current {
                if let Some(node) = contents
                    .iter_mut()
                    .find(|node| node.get_name() == comp.as_os_str())
                {
                    current = node;
                } else {
                    return None;
                };
            } else {
                return None;
            }
        }
        Some(current)
    }

    fn directories(&self) -> Vec<&Self> {
        match self {
            Self::Dir { contents, .. } => {
                let mut v = vec![self];
                contents.iter().for_each(|n| v.append(&mut n.directories()));
                v
            }
            _ => Vec::default(),
        }
    }

    fn get_size(&self) -> u32 {
        match self {
            Self::File { size, .. } => *size,
            Self::Dir { contents, .. } => contents.iter().map(Self::get_size).sum(),
        }
    }
}

fn build_fs_tree(entries: &[Entry]) -> Node {
    let mut pwd = PathBuf::from("/");
    let mut root = Node::Dir {
        name: PathBuf::from("/"),
        contents: vec![],
    };

    for entry in entries {
        match &entry.command {
            Command::Cd { path } => {
                pwd = if path == ".." {
                    pwd.parent().expect("couldn't cd ..").to_path_buf()
                } else {
                    pwd.join(path)
                }
            }
            Command::Ls => {
                for item in &entry.output {
                    if let Some((t, name)) = item.split_once(' ') {
                        let node = if t == "dir" {
                            Node::Dir {
                                name: name.into(),
                                contents: vec![],
                            }
                        } else {
                            Node::File {
                                name: name.into(),
                                size: t.parse::<u32>().expect("invalid file size"),
                            }
                        };
                        if let Node::Dir {
                            ref mut contents, ..
                        } = root
                            .find_node(&pwd)
                            .expect("got lost trying to insert node")
                        {
                            contents.push(node);
                        }
                    } else {
                        panic!("invalid fs entry")
                    }
                }
            }
        }
    }

    root
}
