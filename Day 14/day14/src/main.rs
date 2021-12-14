use std::fs;
use std::collections::HashMap;
use std::iter::{FromIterator,Iterator};
use std::hash::Hash;

fn parse_rules(lines: &[&str]) -> HashMap<(char,char), char> {
    let mut rules = HashMap::new();
    for line in lines {
        let line_chars: Vec<char> = line.chars().collect();
        let pair = (line_chars[0], line_chars[1]);
        let mapped = line_chars[6];

        rules.insert(pair, mapped);
    }

    rules
}

fn parse_input(input: &str) -> (Vec<char>, HashMap<(char,char),char>) {
    let lines: Vec<&str> = input.split("\n").collect();
    let template: Vec<char> = lines[0].chars().collect();
    let rules = parse_rules(&lines[2..]);

    (template, rules)
}

fn get_test_input() -> (Vec<char>, HashMap<(char,char),char>) {
    let template = vec!['N','N','C','B'];

    let rules = HashMap::from_iter([
        (('C','H'), 'B'),
        (('H','H'), 'N'),
        (('C','B'), 'H'),
        (('N','H'), 'C'),
        (('H','B'), 'C'),
        (('H','C'), 'B'),
        (('H','N'), 'C'),
        (('N','N'), 'C'),
        (('B','H'), 'H'),
        (('N','C'), 'B'),
        (('N','B'), 'B'),
        (('B','N'), 'B'),
        (('B','B'), 'N'),
        (('B','C'), 'B'),
        (('C','C'), 'N'),
        (('C','N'), 'C')]);

    (template, rules)
}

struct Counter<T: Eq + Hash>(HashMap<T, usize>);

impl<T: Copy + Eq + Hash> Counter<T> {
    fn increment(&mut self, key: T, incr: usize) {
        if let Some(value) = self.0.get_mut(&key) {
            *value += incr;
        }
        else {
            self.0.insert(key, incr);
        }
    }
}

impl<T: Copy + Eq + Hash> FromIterator<T> for Counter<T> {
    fn from_iter<I: IntoIterator<Item=T>>(list: I) -> Self {
        let mut map = HashMap::<T,usize>::new();
        for x in list {
            if map.contains_key(&x) {
                let count = map.get_mut(&x).unwrap(); 
                *count += 1;
            }
            else {
                map.insert(x, 1);
            }
        }

        Counter(map)
    }
}

fn interpolate(template: &Vec<char>, rules: &HashMap<(char,char),char>) -> Vec<char> {
    let mut interpolated = Vec::<char>::new();
    for i in 0..(template.len()-1) {
        let pair = (template[i], template[i+1]);
        let rule = rules[&pair];
        interpolated.push(pair.0);
        interpolated.push(rule);
    }

    if !template.is_empty() {
        interpolated.push(*template.last().unwrap());
    }

    interpolated
}

fn run(template: &Vec<char>, rules: &HashMap<(char,char),char>, steps: usize) -> Vec<char> {
    let mut interpolated = template.clone();
    for _ in 0..steps {
        interpolated = interpolate(&interpolated, rules);
    }

    interpolated
}

fn part1(template: &Vec<char>, rules: &HashMap<(char,char),char>) -> usize {
    let interpolated = run(template, rules, 10);
    let counter = Counter::from_iter(&interpolated);
    let max = *counter.0.values().into_iter().max().unwrap_or(&0);
    let min = *counter.0.values().into_iter().min().unwrap_or(&0);

    max - min
}

fn run_counter(template: &Vec<char>, rules: &HashMap<(char,char),char>, steps: usize) -> Counter<(char,char)> {
    let mut counter: Counter<(char,char)> = Counter(HashMap::new());
    for i in 0..template.len()-1 {
        let pair = (template[i], template[i+1]);
        counter.increment(pair, 1);
    }
    
    for _ in 0..steps {
        let mut new_counter: Counter<(char,char)> = Counter(HashMap::new());
        
        for (pair,count) in counter.0.iter() {
            let rule = rules[pair];
            let new_pair_1 = (pair.0, rule);
            let new_pair_2 = (rule, pair.1);

            new_counter.increment(new_pair_1, *count);
            new_counter.increment(new_pair_2, *count);
        }

        counter = new_counter;
    }

    counter
}

fn get_char_counter(template: &Vec<char>, pair_counter: Counter<(char,char)>) -> Counter<char> {
    let mut char_counter: Counter<char> = Counter(HashMap::new());
    for ((c1,_),count) in pair_counter.0 {
        char_counter.increment(c1, count);
    }

    if !template.is_empty() {
        let last_count = char_counter.0.get_mut(template.last().unwrap()).unwrap();
        *last_count += 1;
    }

    char_counter
}

fn part2(template: &Vec<char>, rules: &HashMap<(char,char),char>) -> usize {
    let pair_counter = run_counter(template, rules, 40);

    let char_counter = get_char_counter(template, pair_counter);
    let max = *char_counter.0.values().into_iter().max().unwrap_or(&0);
    let min = *char_counter.0.values().into_iter().min().unwrap_or(&0);

    max - min
}

fn main() {
    let file_res = fs::read_to_string("input.txt");
    let (template,rules) = 
        if let Ok(contents) = file_res {
            parse_input(&contents)
        }
        else {
            get_test_input()
        };

    println!("{}", part1(&template, &rules));
    println!("{}", part2(&template, &rules));
}
