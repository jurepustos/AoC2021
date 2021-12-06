use std::fs;

fn get_input(filename: &str) -> Vec<usize> {
    let contents = fs::read_to_string(filename).unwrap();
    let entries: Vec<&str> = contents.split(",").collect();

    entries.iter()
        .map(|entry| entry.parse().unwrap())
        .collect()
}

fn get_test_input() -> Vec<usize> {
    vec![3,4,3,1,2]
}

fn create_counter(numbers: &[usize]) -> Vec<u64> {
    let mut counter = vec![0; 9];
    for &num in numbers {
        counter[num] += 1;
    }

    counter
}

fn growth(input: &[usize], rounds: usize) -> u64 {
    let mut counter = create_counter(input);
    for _ in 0..rounds {
        let zeros = counter[0];
        for i in 1..9 {
            counter[i-1] = counter[i]; 
        }
        counter[6] += zeros;
        counter[8] = zeros;
    }

    counter.iter().sum::<u64>()
}

fn part1(input: &[usize]) {
    println!("{}", growth(input, 80));
}

fn part2(input: &[usize]) {
    println!("{}", growth(input, 256));
}

fn main() {
    let input = get_input("input.txt");
    // let input = get_test_input();

    part1(&input);
    part2(&input);
}
