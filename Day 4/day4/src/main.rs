use std::fs;

struct Board {
    lines: Vec<Vec<usize>>,
    row_covers: Vec<Vec<usize>>,
    column_covers: Vec<Vec<usize>>
}

impl Board {
    fn new(lines: Vec<Vec<usize>>) -> Self {
        let len = lines.len();
        Board {
            lines,
            row_covers: vec![Vec::new(); len],
            column_covers: vec![Vec::new(); len]
        }
    }

    fn reset(&mut self) {
        let len = self.lines.len();
        self.row_covers = vec![Vec::new(); len];
        self.column_covers = vec![Vec::new(); len]
    }

    fn cover(&mut self, number: usize) {
        for (i,line) in self.lines.iter().enumerate() {
            for (j,entry) in line.iter().enumerate() {
                if *entry == number 
                && !self.row_covers[i].contains(entry)
                && !self.column_covers[j].contains(entry) {
                    self.row_covers[i].push(*entry);
                    self.column_covers[j].push(*entry);
                }
            }
        }
    }

    fn is_finished(&self) -> bool {
        let mut finished = false;
        for i in 0..self.lines.len() {
            if self.row_covers[i].len() == self.lines.len()
            || self.column_covers[i].len() == self.lines.len() {
                finished = true;
                break;
            }
        }
        
        finished
    }

    fn uncovered_sum(&self) -> usize {
        let mut sum = 0;
        for (i,line) in self.lines.iter().enumerate() {
            for (j,entry) in line.iter().enumerate() {
                if !self.row_covers[i].contains(entry)
                || !self.column_covers[j].contains(entry) {
                    sum += entry;
                }
            }
        }

        sum
    }
}

fn get_input(filename: &str) -> (Vec<usize>, Vec<Board>) {
    let contents = fs::read_to_string(filename).unwrap();
    let content_lines: Vec<&str> = contents.split("\n").collect();

    let numbers = parse_numbers(content_lines[0]);

    let mut line_index = 2;
    let mut boards = Vec::new();
    while line_index < content_lines.len() {
        let board = parse_board(&content_lines[line_index..line_index+5]);
        boards.push(board);
        line_index += 6;
    }

    (numbers, boards)
}

fn parse_numbers(line: &str) -> Vec<usize> {
    line.split(",")
        .map(|num_str| num_str.parse().unwrap())
        .collect()
}

fn parse_board(lines: &[&str]) -> Board {
    let mut number_lines = Vec::new();
    for line in lines {
        let num_strs: Vec<&str> = line.split(" ")
            .into_iter()
            .filter(|num_str| num_str != &"")
            .collect();

        number_lines.push(num_strs.iter()
            .map(|num_str| num_str.parse().unwrap())
            .collect());
    }

    Board::new(number_lines)
}

fn get_test_input() -> (Vec<usize>, Vec<Board>) {
    let numbers = vec![7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1];
    let boards = vec![
        // Board::new(
        //     vec![
        //         vec![22, 13, 17, 11, 0],
        //         vec![8, 2, 23, 4, 24],
        //         vec![21, 9, 14, 16, 7],
        //         vec![6, 10, 3, 18, 5],
        //         vec![1, 12, 20, 15, 19],
        //     ]),
        Board::new(
            vec![
                vec![3, 15, 0, 2, 22],
                vec![9, 18, 13, 17, 5],
                vec![19, 8, 7, 25, 23],
                vec![20, 11, 10, 24, 4],
                vec![14, 21, 16, 12, 6]
            ]),
        Board::new(
            vec![
                vec![14, 21, 17, 24, 4],
                vec![10, 16, 15, 9, 19],
                vec![18, 8, 23, 26, 20],
                vec![22, 11, 13, 6, 5],
                vec![2, 0, 12, 3, 7]
            ])
    ];

    (numbers, boards)
}

fn cover_board(board: &mut Board, num: usize) -> usize {
    let mut score = 0;
    board.cover(num);
    if board.is_finished() {
        let board_sum = board.uncovered_sum();
        score = board_sum * num;
    }

    score
}

fn part1(numbers: &Vec<usize>, boards: &mut Vec<Board>) {
    let mut board_score = 0;
    for &num in numbers {
        for board in boards.iter_mut() {
            board_score = cover_board(board, num);
            if board_score > 0 {
                break;
            }
        }

        if board_score > 0 {
            break;
        }
    }   

    println!("{}", board_score);
}

fn part2(numbers: &Vec<usize>, boards: &mut Vec<Board>) {
    let boards_count = boards.len();
    let mut last_board_score = 0;
    let mut remaining_boards = vec![true; boards_count];
    let mut remaining_count = boards_count;
    for &num in numbers {
        for (i,board) in boards.iter_mut().enumerate() {
            if remaining_count >= 1 && remaining_boards[i] {
                let board_score = cover_board(board, num);
                if board_score > 0 {
                    last_board_score = board_score;
                    remaining_boards[i] = false;
                    remaining_count -= 1;
                }
            }
        }

        if remaining_count == 0 {
            break;
        }
    }   

    println!("{}", last_board_score);
}

fn main() {
    // let (numbers, mut boards) = get_test_input();
    let (numbers, mut boards) = get_input("input.txt");

    part1(&numbers, &mut boards);
    for board in boards.iter_mut() {
        board.reset();
    }
    part2(&numbers, &mut boards);
}
