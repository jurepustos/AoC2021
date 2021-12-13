use std::fs;
use std::collections::HashSet;
use std::iter::{Iterator,FromIterator};


enum Fold {
    X(usize),
    Y(usize)
}

fn parse_dot(line: &str) -> (usize,usize) {
    let pair: Vec<&str> = line.split(",").collect();
    (pair[0].parse().unwrap(),pair[1].parse().unwrap())
}

fn parse_fold(line: &str) -> Fold {
    let part = line.split(" ").nth(2).unwrap();
    let chars: Vec<char> = part.chars().collect();
    let number: String = chars[2..].iter().collect();
    if chars[0] == 'x' {
        Fold::X(number.parse().unwrap())
    }
    else {
        Fold::Y(number.parse().unwrap())
    }
}

fn parse_input(input: &str) -> (HashSet<(usize,usize)>, Vec<Fold>) {
    let lines: Vec<&str> = input.split("\n").collect();

    let mut dots = HashSet::<(usize,usize)>::new();
    let mut folds = Vec::<Fold>::new();

    let mut phase = "dots";
    for line in lines {
        if line.is_empty() {
            phase = "folds";
        }
        else {
            if phase == "dots" {
                dots.insert(parse_dot(line));
            }
            else {
                folds.push(parse_fold(line));
            }
        }
    }

    (dots,folds)
}

fn get_test_input() -> (HashSet<(usize,usize)>, Vec<Fold>) {
    let dots = HashSet::from_iter([
        (6,10),
        (0,14),
        (9,10),
        (0,3),
        (10,4),
        (4,11),
        (6,0),
        (6,12),
        (4,1),
        (0,13),
        (10,12),
        (3,4),
        (3,0),
        (8,4),
        (1,10),
        (2,14),
        (8,10),
        (9,0),
    ]);

    let folds = vec![
        Fold::Y(7),
        Fold::X(5)
    ];

    (dots, folds)
}

fn fold_dots(dots: &HashSet<(usize,usize)>, fold: &Fold) -> HashSet<(usize,usize)> {
    let mut folded_dots = HashSet::<(usize,usize)>::new(); 
    for (x,y) in dots {
        match fold {
            Fold::X(fold_x) => {
                if *x < *fold_x {
                    folded_dots.insert((*x,*y));
                }
                else {
                    let new_x = (fold_x - (x % fold_x)) % fold_x;
                    folded_dots.insert((new_x,*y));
                }
            },
            Fold::Y(fold_y) => {
                if *y < *fold_y {
                    folded_dots.insert((*x,*y));
                }
                else {
                    let new_y = (fold_y - (y % fold_y)) % fold_y;
                    folded_dots.insert((*x,new_y));
                }
            }
        }
    }

    folded_dots
}

fn part1(dots: &HashSet<(usize,usize)>, fold: &Fold) -> HashSet<(usize,usize)> {
    fold_dots(dots, fold)
}

fn dot_grid(dots: &HashSet<(usize,usize)>) -> Vec<Vec<char>> {
    let max_x = dots.iter().map(|(x,_)| *x).max().unwrap_or(0);
    let max_y = dots.iter().map(|(_,y)| *y).max().unwrap_or(0);

    let mut grid: Vec<Vec<char>> = vec![vec!['.'; max_x+1]; max_y+1];
    for (x,y) in dots {
        grid[*y][*x] = '#';
    }

    grid
}

fn display_grid(grid: &Vec<Vec<char>>) -> String {
    let mut display = String::new();
    for line in grid {
        for c in line {
            display.push(*c);
        }
        display.push('\n');
    }
    display
}

fn part2(dots: &HashSet<(usize,usize)>, folds: &Vec<Fold>) -> HashSet<(usize,usize)> {
    let mut folded_dots = None; 
    for fold in folds {
        match folded_dots {
            None => folded_dots = Some(fold_dots(dots, fold)),
            Some(folded) => folded_dots = Some(fold_dots(&folded, fold)),
        }
    }

    folded_dots.unwrap_or(HashSet::new())
}

fn main() {
    let file_res = fs::read_to_string("input.txt");
    let (dots,folds): (HashSet<(usize,usize)>,Vec<Fold>) = 
        if let Ok(contents) = file_res {
            parse_input(&contents)
        }
        else {
            get_test_input()
        };

    let part1_sol = part1(&dots, &folds[0]);
    let part2_sol = part2(&dots, &folds);
    println!("{}", part1_sol.len());
    println!("{}", display_grid(&dot_grid(&part2_sol)));
}
