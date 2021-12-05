use std::fs;

#[derive(Clone,Copy,Debug)]
struct Coord {
    x: usize,
    y: usize
}

impl Coord {
    fn new(x: usize, y: usize) -> Self {
        Coord { x,y }
    }
}

#[derive(Clone,Copy,Debug)]
struct Line {
    start: Coord,
    end: Coord
}

impl Line {
    fn new(start: Coord, end: Coord) -> Self {
        Line { start,end }
    }
}

fn get_input(filename: &str) -> Vec<Line> {
    let contents = fs::read_to_string(filename).unwrap();
    let file_lines: Vec<&str> = contents.split("\n")
        .collect();

    let mut input_lines = Vec::new();
    for line in file_lines {
        let start_end: Vec<&str> = line.split(" -> ").collect();
        let start_parts: Vec<&str> = start_end[0].split(",").collect();
        let end_parts: Vec<&str> = start_end[1].split(",").collect();

        let start_x: usize = start_parts[0].parse().unwrap();
        let start_y: usize = start_parts[1].parse().unwrap();

        let end_x: usize = end_parts[0].parse().unwrap();
        let end_y: usize = end_parts[1].parse().unwrap();

        let line = Line::new(Coord::new(start_x,start_y), Coord::new(end_x,end_y));
        input_lines.push(line);
    }

    input_lines
}

fn get_test_input() -> Vec<Line> {
    vec![
        Line::new(Coord::new(0,9),Coord::new(5,9)),
        Line::new(Coord::new(8,0),Coord::new(0,8)),
        Line::new(Coord::new(9,4),Coord::new(3,4)),
        Line::new(Coord::new(2,2),Coord::new(2,1)),
        Line::new(Coord::new(7,0),Coord::new(7,4)),
        Line::new(Coord::new(6,4),Coord::new(2,0)),
        Line::new(Coord::new(0,9),Coord::new(2,9)),
        Line::new(Coord::new(3,4),Coord::new(1,4)),
        Line::new(Coord::new(0,0),Coord::new(8,8)),
        Line::new(Coord::new(5,5),Coord::new(8,2))
    ]
}

fn line_max(line: Line) -> usize {
    [line.start.x,line.start.y,line.end.x,line.end.y]
        .iter()
        .max()
        .map(|&num| num)
        .unwrap_or(0)
}

fn generate_grid(lines: &Vec<&Line>) -> Vec<Vec<usize>> {
    let max = lines.iter()
        .map(|&&line| line_max(line))
        .max()
        .unwrap_or(0);

    let mut grid = vec![vec![0; max+1]; max+1];
    for line in lines {
        let min_x = *[line.start.x, line.end.x].iter().min().unwrap();
        let max_x = *[line.start.x, line.end.x].iter().max().unwrap();
        let min_y = *[line.start.y, line.end.y].iter().min().unwrap();
        let max_y = *[line.start.y, line.end.y].iter().max().unwrap();

        if line.start.x == line.end.x {
            for y in min_y..=max_y {
                grid[y][line.start.x] += 1;
            }
        }
        else if line.start.y == line.end.y {
            for x in min_x..=max_x {
                grid[line.start.y][x] += 1;
            }
        }
        else if line.start.x < line.end.x {
            let mut x = line.start.x;
            let mut y = line.start.y;
            if line.start.y < line.end.y {
                while x <= line.end.x && y <= line.end.y {
                    grid[y][x] += 1;
                    x += 1;
                    y += 1;
                }
            }
            else if line.start.y > line.end.y {
                while x <= line.end.x && y >= line.end.y {
                    grid[y][x] += 1;
                    x += 1;
                    if y >= 1 {
                        y -= 1;
                    }
                }
            }
        }
        else if line.start.x > line.end.x {
            let mut x = line.start.x;
            let mut y = line.start.y;
            if line.start.y < line.end.y {
                while x >= line.end.x && y <= line.end.y {
                    grid[y][x] += 1;
                    if x >= 1 {
                        x -= 1;
                    }
                    y += 1;
                }
            }
            else if line.start.y > line.end.y {
                while x >= line.end.x && y >= line.end.y {
                    grid[y][x] += 1;
                    if x >= 1 {
                        x -= 1;
                    }
                    if y >= 1 {
                        y -= 1;
                    }
                }
            }
        }
    }

    grid
}

fn part1(input: &Vec<Line>) {
    let straight_lines: Vec<&Line> = input.iter()
        .filter(|line| line.start.x == line.end.x || line.start.y == line.end.y)
        .collect();

    let grid = generate_grid(&straight_lines);
    let mut count = 0;
    for x in 0..grid.len() {
        for y in 0..grid.len() {
            if grid[x][y] >= 2 {
                count += 1;
            }
        }
    }

    println!("{}", count);
}

fn part2(input: &Vec<Line>) {
    let lines = input.iter().collect();

    let grid = generate_grid(&lines);
    let mut count = 0;
    for x in 0..grid.len() {
        for y in 0..grid.len() {
            if grid[x][y] >= 2 {
                count += 1;
            }
        }
    }

    println!("{}", count);
}

fn main() {
    // let input = get_test_input();
    let input = get_input("input.txt");

    part1(&input);
    part2(&input);
}
