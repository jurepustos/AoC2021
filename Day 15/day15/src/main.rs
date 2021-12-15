use std::fs;
use std::collections::{VecDeque,HashSet,BinaryHeap};
use std::hash::{Hash,Hasher};
use std::cmp::Reverse;

fn parse_input(input: &str) -> Vec<Vec<usize>> {
    let lines: Vec<&str> = input.split("\n").collect();
    let mut grid = Vec::<Vec<usize>>::new();
    for line in lines {
        let line_nums: Vec<usize> = line.chars()
            .map(|c| c.to_digit(10).unwrap_or(0) as usize)
            .collect();
        grid.push(line_nums);
    }

    grid
}

fn get_test_input() -> Vec<Vec<usize>> {
    vec![
        vec![1,1,6,3,7,5,1,7,4,2],
        vec![1,3,8,1,3,7,3,6,7,2],
        vec![2,1,3,6,5,1,1,3,2,8],
        vec![3,6,9,4,9,3,1,5,6,9],
        vec![7,4,6,3,4,1,7,1,1,1],
        vec![1,3,1,9,1,2,8,1,3,7],
        vec![1,3,5,9,9,1,2,4,2,1],
        vec![3,1,2,5,4,2,1,6,3,9],
        vec![1,2,9,3,1,3,8,5,2,1],
        vec![2,3,1,1,9,4,4,5,8,1],
    ]
}

fn get_neighbors(grid: &Vec<Vec<usize>>, x: usize, y: usize) -> Vec<(usize,usize)> {
    let n = grid.len();
    let m = grid.get(0).map(|line| line.len()).unwrap_or(0);

    let mut neighbors = Vec::new();
    if x > 0 {
        neighbors.push((x-1,y));
    }

    if x < m-1 {
        neighbors.push((x+1,y));
    }

    if y > 0 {
        neighbors.push((x,y-1));
    }

    if y < n-1 {
        neighbors.push((x,y+1));
    }

    neighbors
}

#[derive(Clone,Copy,Debug)]
struct Point {
    prev: (usize,usize),
    x: usize,
    y: usize,
    cost: usize
}

impl Point {
    fn from(grid: &Vec<Vec<usize>>, prev_point: &Point, x: usize, y: usize) -> Self {
        Point {
            prev: (prev_point.x,prev_point.y),
            x,
            y,
            cost: prev_point.cost + grid[y][x]
        }
    }
}

impl PartialOrd<Self> for Point { 
    fn partial_cmp(&self, other: &Self) -> std::option::Option<std::cmp::Ordering> {
        self.cost.partial_cmp(&other.cost)
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost.cmp(&other.cost)
    }
}

impl PartialEq<Self> for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}

impl Eq for Point {
}

impl Hash for Point {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.x.hash(state);
        self.y.hash(state);
    }
}

fn shortest_path_cost(grid: &Vec<Vec<usize>>) -> usize {
    let n = grid.len();
    let m = grid.get(0).and_then(|line| Some(line.len())).unwrap_or(1);

    let origin = Point {
        prev: (m,n),
        x: 0,
        y: 0,
        cost: 0
    };

    let mut total_cost = None;
    let mut points = HashSet::new();
    let mut heap = BinaryHeap::new();
    points.insert(origin);
    heap.push(Reverse(origin));
    while let Some(Reverse(point)) = heap.pop() {
        println!("{:?}", point);
        if point.y == n-1
        && point.x == m-1 {
            total_cost = Some(point.cost);
            break;
        }

        let neighbors = get_neighbors(grid, point.x, point.y)
            .into_iter()
            .map(|(x,y)| Point::from(grid, &point, x, y));
        for neighbor in neighbors {
            if !points.contains(&neighbor) {
                heap.push(Reverse(neighbor));
                points.insert(neighbor);
            }
        }
    }

    total_cost.unwrap_or(0)
}

fn part1(grid: &Vec<Vec<usize>>) -> usize {
    shortest_path_cost(grid);
}

fn part2(grid: &Vec<Vec<usize>>) -> usize {
    unimplemented!()
}

fn main() {
    let file_res = fs::read_to_string("input.txt");
    let grid = 
        if let Ok(contents) = file_res {
            parse_input(&contents)
        }
        else {
            get_test_input()
        };

    println!("{}", part1(&grid));
    println!("{}", part2(&grid));
}
