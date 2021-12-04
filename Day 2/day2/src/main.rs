use std::fs;

fn get_input(filename: &str) -> Vec<Command> {
    let contents = fs::read_to_string(filename).unwrap();
    contents.split("\n")
        .map(|line| parse_line(line))
        .collect()
}

fn get_test_input() -> Vec<Command> {
    let input = vec![
        "forward 5",
        "down 5",
        "forward 8",
        "up 3",
        "down 8",
        "forward 2"
    ];

    input.iter()
        .map(|line| parse_line(line))
        .collect()
}

fn parse_line(line: &str) -> Command {
    let parts: Vec<&str> = line.split(" ").collect();
    let com_str = parts[0];
    let num: usize = parts[1].parse().unwrap();

    match com_str {
        "forward" => Command::Forward(num),
        "up" => Command::Up(num),
        "down" => Command::Down(num),
        _ => Command::Nothing
    }
}

enum Command {
    Nothing,
    Forward(usize),
    Up(usize),
    Down(usize)
}

fn part1(commands: &Vec<Command>) {
    let mut pos: usize = 0;
    let mut depth: isize = 0;

    for command in commands {
        match command {
            Command::Nothing => (),
            Command::Forward(x) => {
                pos += x;
            },
            Command::Up(y) => {
                depth -= *y as isize;
            },
            Command::Down(y) => {
                depth += *y as isize;
            }
        };
    }

    println!("{}", (pos as isize)*depth);
}

fn part2(commands: &Vec<Command>) {
    let mut pos: usize = 0;
    let mut depth: isize = 0;
    let mut aim: isize = 0;

    for command in commands {
        match command {
            Command::Nothing => (),
            Command::Up(y) => {
                aim -= *y as isize;
            },
            Command::Down(y) => {
                aim += *y as isize;
            },
            Command::Forward(x) => {
                pos += x;
                depth += aim*(*x as isize);
            }
        };
    }

    println!("{}", (pos as isize)*depth)
}

fn main() {
    let commands = get_input("input.txt");

    part1(&commands);
    part2(&commands);
}
