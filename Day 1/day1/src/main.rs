use std::fs;

fn get_input(filename: &str) -> Vec<usize> {
    let input = fs::read_to_string(filename).expect("canot read file input.txt");
    input.split("\n")
        .map(|line| line.parse().unwrap_or(0))
        .collect()
}

fn count_increases(list: &Vec<usize>) -> usize {
    let mut count = 0;
    let mut current = list[0];
    for i in 1..list.len() {
        let next = list[i];
        if current < next {
            count += 1;
        }
        current = next;
    }
    count
}

fn part1(heights: &Vec<usize>) {
    let count = count_increases(heights);
    println!("{}", count);
}

fn get_windows(heights: &Vec<usize>) -> Vec<usize> {
    let mut windows: Vec<usize> = Vec::new();
    for i in 0..(heights.len()-2) {
        let window = heights[i] + heights[i+1] + heights[i+2];
        windows.push(window);
    }
    windows
}

fn part2(heights: &Vec<usize>) {
    let windows = get_windows(heights);
    let count = count_increases(&windows);
    println!("{}", count);
}

fn main() {
    let filename = "input.txt";
    let heights = get_input(filename);

    part1(&heights);
    part2(&heights);
}

