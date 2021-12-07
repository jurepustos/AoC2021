use std::fs;

fn get_input(filename: &str) -> Vec<usize> {
    let contents = fs::read_to_string(filename).unwrap();
    let entries: Vec<&str> = contents.split(",").collect();
    entries.iter()
        .map(|entry| entry.parse().unwrap())
        .collect()
}

fn get_test_input() -> Vec<usize> {
    vec![16,1,2,0,4,2,7,1,2,14]
}

fn part1(input: &[usize]) -> usize {
    let min = *input.iter().min().unwrap_or(&0);
    let max = *input.iter().max().unwrap_or(&0);

    let mut best_cost = usize::MAX;
    for i in min..=max {
        let cost: usize = input.iter()
            .map(|&pos| isize::abs((pos as isize) - (i as isize)) as usize)
            .sum();

        if cost < best_cost {
            best_cost = cost;
        }
    }

    best_cost
}

fn part2(input: &[usize]) -> usize {
    let min = *input.iter().min().unwrap_or(&0);
    let max = *input.iter().max().unwrap_or(&0);

    let mut best_cost = usize::MAX;
    for i in min..=max {
        let cost: usize = input.iter()
            .map(|&pos| isize::abs((pos as isize) - (i as isize)) as usize)
            .map(|diff| diff*(diff+1)/2)
            .sum();

        if cost < best_cost {
            best_cost = cost;
        }
    }

    best_cost
}

fn main() {
    let input = get_input("input.txt");
    // let input = get_test_input();

    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
