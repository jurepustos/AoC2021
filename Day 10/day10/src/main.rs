use std::fs;
use std::collections::HashMap;
use std::iter::FromIterator;

fn parse_input(contents: &str) -> Vec<Vec<char>> {
    let lines: Vec<&str> = contents.split("\n").collect();
    lines.into_iter()
        .map(|line| line.chars().collect())
        .collect()
}

fn get_test_input() -> Vec<Vec<char>> {
    let lines = vec![
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]",
    ];

    lines.into_iter()
        .map(|line| line.chars().collect())
        .collect()
}

fn error_scores(lines: &Vec<Vec<char>>) -> (usize, usize) {
    const OPENING_BRACKETS: [char; 4] = ['[', '(', '{', '<'];
    let closing_brackets: HashMap<char,char> = HashMap::from_iter([
        ('[', ']'),
        ('{', '}'),
        ('(', ')'),
        ('<', '>')
    ]);

    let mut error_score = 0;
    let mut autocomplete_scores: Vec<usize> = Vec::new();
    for line in lines {
        let n = line.len();
        let mut stack: Vec<char> = Vec::new();
        let mut i = 0;
        let mut accepting = true;
        while i == 0 || (i < n && !stack.is_empty()) {
            let next = line[i];
            if OPENING_BRACKETS.contains(&next) {
                stack.push(next);
                i += 1;
            }
            else {
                let opening = stack.pop().unwrap();
                let closing = closing_brackets[&opening];
                if next != closing {
                    accepting = false;
                    break;
                }
                else {
                    i += 1;
                }
            }
        }

        if !accepting {
            let char_scores: HashMap<char, usize> = HashMap::from_iter([
                (')', 3),
                (']', 57),
                ('}', 1197),
                ('>', 25137)
            ]);
            let illegal_char = line[i];
            error_score += char_scores[&illegal_char];
        }
        else {
            let mut autocomplete_score = 0;
            let autocomplete: HashMap<char, usize> = HashMap::from_iter([
                ('(', 1),
                ('[', 2),
                ('{', 3),
                ('<', 4)
            ]);
            while !stack.is_empty() {
                let unmatched_char = stack.pop().unwrap();
                autocomplete_score *= 5;
                autocomplete_score += autocomplete[&unmatched_char];
            }

            if autocomplete_score > 0 {
                autocomplete_scores.push(autocomplete_score);
            }
        }

    }

    autocomplete_scores.sort();
    let autocomplete_count = autocomplete_scores.len();

    (error_score, autocomplete_scores[autocomplete_count / 2])
}

fn main() {
    let file_res = fs::read_to_string("input.txt");
    let input: Vec<Vec<char>> = 
        if let Ok(contents) = file_res {
            parse_input(&contents)
        }
        else {
            get_test_input()
        };

    let scores = error_scores(&input);  
    println!("{}\n{}", scores.0, scores.1);
}


