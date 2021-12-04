use std::fs;

fn get_input(filename: &str) -> Vec<Vec<char>> {
    let contents = fs::read_to_string(filename).unwrap();
    let lines: Vec<&str> = contents.split("\n").collect();
    lines.iter()
        .map(|line| {
            let len = line.len();
            if line.chars().last().unwrap() == '\r' {
                &line[..len-1]
            }
            else {
                &line
            }
        })
        .map(|line| line.chars().collect())
        .collect()
}

fn get_test_input() -> Vec<Vec<char>> {
    vec![
        "00100".chars().collect(),
        "11110".chars().collect(),
        "10110".chars().collect(),
        "10111".chars().collect(),
        "10101".chars().collect(),
        "01111".chars().collect(),
        "00111".chars().collect(),
        "11100".chars().collect(),
        "10000".chars().collect(),
        "11001".chars().collect(),
        "00010".chars().collect(),
        "01010".chars().collect()
    ]
}

fn count_ones(chars: &Vec<&Vec<char>>, line_length: usize) -> Vec<usize> {
    let mut counters: Vec<usize> = vec![0; line_length];
    for word in chars {
        for i in 0..line_length {
            if word[i] == '1' {
                counters[i] += 1;
            }
        }
    }

    counters
}

fn get_diag_codes(counters: &Vec<usize>, input_length: usize) -> (usize, usize) {
    let mut gamma_chars: Vec<char> = Vec::new();
    let mut epsilon_chars: Vec<char> = Vec::new();
    for &count in counters.iter() {
        if count > input_length / 2 {
            gamma_chars.push('1');
            epsilon_chars.push('0');
        }
        else {
            gamma_chars.push('0');
            epsilon_chars.push('1');
        }
    }

    let gamma_str: String = gamma_chars.iter().collect();
    let epsilon_str: String = epsilon_chars.iter().collect();

    let gamma = usize::from_str_radix(&gamma_str,2).unwrap();
    let epsilon = usize::from_str_radix(&epsilon_str,2).unwrap();

    (gamma, epsilon)
}

fn part1(input: &Vec<Vec<char>>) {
    let line_length = input[0].len();
    let input_length = input.len();

    let input_refs = input.iter().map(|word| word).collect();

    let counters = count_ones(&input_refs, line_length);
    let (gamma, epsilon) = get_diag_codes(&counters, input_length);

    println!("{}", gamma*epsilon);
}

fn get_life_rating(input: &Vec<Vec<char>>, majority_char: char, minority_char: char) -> usize {
    let line_length = input[0].len();

    let mut remaining: Vec<&Vec<char>> = input.iter()
        .map(|word| word)
        .collect();

    for i in 0..line_length {
        let counters = count_ones(&remaining, line_length);
        if counters[i] as f32 >= remaining.len() as f32 / 2 as f32 {
            // 1 is the most common number
            remaining = remaining.into_iter()
                .filter(|word| word[i] == majority_char)
                .collect();
        }
        else {
            remaining = remaining.into_iter()
                .filter(|word| word[i] == minority_char)
                .collect();
        }

        if remaining.len() == 1 {
            break;
        }
    }

    let rating_str: String = remaining[0].iter().collect();
    usize::from_str_radix(&rating_str,2).unwrap()
}

fn get_oxygen_rating(input: &Vec<Vec<char>>) -> usize {
    get_life_rating(&input, '1', '0')
}

fn get_co2_rating(input: &Vec<Vec<char>>) -> usize {
    get_life_rating(&input, '0', '1')
}

fn part2(input: &Vec<Vec<char>>) {
    let oxygen = get_oxygen_rating(&input);
    let co2 = get_co2_rating(&input);

    println!("{}", oxygen*co2);
}

fn main() {
    let input = get_input("input.txt");
    // let input = get_test_input();

    part1(&input); 
    part2(&input);
}
