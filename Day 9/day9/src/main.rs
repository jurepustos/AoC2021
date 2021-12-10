use std::fs;
use std::collections::HashSet;


#[derive(PartialEq,Eq,Clone,Copy,Hash,Debug)]
struct Point {
    row: usize,
    column: usize,
    value: usize
}

fn get_input(contents: &str) -> Vec<Vec<usize>> {
    let mut heightmap: Vec<Vec<usize>> = Vec::new();

    let lines: Vec<&str> = contents.split("\n").collect();
    for line in lines {
        let mut line_heights: Vec<usize> = Vec::new();
        for c in line.chars() {
            line_heights.push(c.to_digit(10).unwrap() as usize);
        }

        heightmap.push(line_heights);
    }

    heightmap
}

fn get_test_input() -> Vec<Vec<usize>> {
    vec![
        vec![2,1,9,9,9,4,3,2,1,0],
        vec![3,9,8,7,8,9,4,9,2,1],
        vec![9,8,5,6,7,8,9,8,9,2],
        vec![8,7,6,7,8,9,6,7,8,9],
        vec![9,8,9,9,9,6,5,6,7,8]
    ]
}

fn get_neighbor_points(heightmap: &[Vec<usize>], row: usize, column: usize) -> Vec<Point> {
    let n = heightmap.len();
    let m = heightmap[0].len();

    let mut neighbors: Vec<Point> = Vec::new();
    if row > 0 {
        neighbors.push(Point { 
            row: row-1, 
            column: column,
            value: heightmap[row-1][column]
        });
    }

    if row < n-1 {
        neighbors.push(Point { 
            row: row+1, 
            column: column,
            value: heightmap[row+1][column]
        });
    }

    if column > 0 {
        neighbors.push(Point { 
            row: row, 
            column: column-1,
            value: heightmap[row][column-1]
        });
    }

    if column < m-1 {
        neighbors.push(Point { 
            row: row, 
            column: column+1,
            value: heightmap[row][column+1]
        });
    }

    neighbors
}

fn is_low_point(heightmap: &[Vec<usize>], row: usize, column: usize) -> bool {
    let neighbors = get_neighbor_points(heightmap, row, column);
    let value = heightmap[row][column];
    neighbors.iter().all(|point| point.value > value)
}


fn get_low_points(heightmap: &[Vec<usize>]) -> Vec<Point> {
    let n = heightmap.len();
    let m = heightmap[0].len();    

    let mut low_points: Vec<Point> = Vec::new();
    for i in 0..n {
        for j in 0..m {
            if is_low_point(heightmap, i, j) {
                let point = Point {
                    row: i,
                    column: j,
                    value: heightmap[i][j]
                };
                low_points.push(point);
            }
        }
    }

    low_points
}

fn part1(heightmap: &[Vec<usize>]) -> usize {
    let low_points = get_low_points(heightmap);
    low_points.iter().map(|point| point.value+1).sum::<usize>()
}

fn get_basin(heightmap: &[Vec<usize>], row: usize, column: usize) -> HashSet<Point> {
    let low_point = Point {
        row, 
        column,
        value: heightmap[row][column]
    };

    let mut basin_points: HashSet<Point> = HashSet::new();
    basin_points.insert(low_point);

    let mut new_points: HashSet<Point> = HashSet::new();
    let neighbors = get_neighbor_points(heightmap, row, column);
    for point in neighbors {
        if point.value < 9 {
            new_points.insert(point);
            basin_points.insert(point);
        }
    }

    // very dumb breadth-first search
    while !new_points.is_empty() {
        let mut neighbors: HashSet<Point> = HashSet::new();
        for point in new_points.iter() {
            let point_neighbors = get_neighbor_points(heightmap, point.row, point.column);
            for new_point in point_neighbors {
                if new_point.value < 9 {
                    if basin_points.insert(new_point) {
                        neighbors.insert(new_point);
                    }
                }
            }
        }
        new_points = neighbors;
    }

    basin_points
}

fn part2(heightmap: &[Vec<usize>]) -> usize {
    let low_points: Vec<Point> = get_low_points(heightmap);

    let mut basins: Vec<HashSet<Point>> = Vec::new();
    for point in low_points {
        let basin = get_basin(heightmap, point.row, point.column);
        basins.push(basin);
    }

    basins.sort_unstable_by(|b1, b2| b2.len().cmp(&b1.len()));

    basins[0].len() * basins[1].len() * basins[2].len()
}

fn main() {
    let file_res = fs::read_to_string("input.txt");
    let input: Vec<Vec<usize>> = 
        if let Ok(contents) = file_res {
            get_input(&contents)
        }
        else {
            get_test_input()
        };

    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
