use std::fs;
use std::collections::HashSet;

fn parse_line(line: &str) -> Vec<usize> {
    line.chars()
        .map(|c| c.to_digit(10).unwrap() as usize)
        .collect()
}

fn parse_input(contents: &str) -> Vec<Vec<usize>> {
    contents.split("\n")
        .into_iter()
        .map(parse_line)
        .collect()
}

fn get_test_input() -> Vec<Vec<usize>> {
    let lines = vec![
        "5483143223",
        "2745854711",
        "5264556173",
        "6141336146",
        "6357385478",
        "4167524645",
        "2176841721",
        "6882881134",
        "4846848554",
        "5283751526",
    ];

    lines.into_iter()
        .map(parse_line)
        .collect()
}

struct Grid(Vec<Vec<usize>>);

impl Grid {
    fn step(&mut self) -> usize {
        let mut flashes: HashSet<(usize,usize)> = HashSet::new();
        
        let mut flashing_stack: Vec<(usize,usize)> = Vec::new();
        for row in 0..self.0.len() {
            for column in 0..self.0[row].len() {
                self.0[row][column] += 1;
                if self.0[row][column] > 9 {
                    flashing_stack.push((row,column));
                }
            }
        }

        while !flashing_stack.is_empty() {
            let (row,column) = flashing_stack.pop().unwrap();
            if flashes.insert((row,column)) {
                let neighbors = self.get_neighbors(row, column);
                for (x,y) in neighbors {
                    self.0[x][y] += 1;
                    if self.0[x][y] > 9 {
                        flashing_stack.push((x,y));
                    }
                }
            }
        }

        for &(row,column) in &flashes {
            self.0[row][column] = 0;
        } 
        
        flashes.len()
    }

    fn get_neighbors(&self, row: usize, column: usize) -> Vec<(usize,usize)> {
        let n = self.0.len();
        let m = self.0.get(0).unwrap_or(&Vec::new()).len();

        let mut neighbors = Vec::new();
        if row > 0 {
            if column > 0 {
                neighbors.push((row-1,column-1));
            }
            if column < m-1 {
                neighbors.push((row-1,column+1));
            }

            neighbors.push((row-1,column));
        }
        if row < n-1 {
            if column > 0 {
                neighbors.push((row+1,column-1));
            }
            if column < m-1 {
                neighbors.push((row+1,column+1));
            }

            neighbors.push((row+1,column));
        }

        if column > 0 {
            neighbors.push((row,column-1));
        }
        if column < m-1 {
            neighbors.push((row,column+1));
        }

        neighbors
    }
}

fn part1(table: Vec<Vec<usize>>) -> usize {
    let mut grid = Grid(table.clone());
    let mut flash_count = 0;
    for _ in 0..100 {
        flash_count += grid.step();
    }

    flash_count
}

fn part2(table: Vec<Vec<usize>>) -> usize {
    let mut grid = Grid(table.clone());
    let grid_elem_count = grid.0.len() * grid.0.get(0).unwrap_or(&Vec::new()).len();
    let mut last_flash_count = 0;
    let mut step_index = 0;
    while last_flash_count < grid_elem_count {
        step_index += 1;
        last_flash_count = grid.step();
    }

    step_index
}

fn main() {
    let file_res = fs::read_to_string("input.txt");
    let grid_table: Vec<Vec<usize>> = 
        if let Ok(contents) = file_res {
            parse_input(&contents)
        }
        else {
            get_test_input()
        };

    println!("{}", part1(grid_table.clone()));
    println!("{}", part2(grid_table.clone()));
}
