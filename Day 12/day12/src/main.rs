use std::fs;
use std::collections::HashMap;

fn parse_input(input: &str) -> Vec<(String,String)> {
    let lines = input.split("\n");
    let mut pairs: Vec<(String,String)> = Vec::new();
    for line in lines {
        let pair: Vec<&str> = line.split("-").collect();
        pairs.push((pair[0].to_string(), pair[1].to_string())); 
    }

    pairs
}

fn get_test_input()-> Vec<(String,String)> {
    vec![
        ("start","A"),
        ("start","b"),
        ("A","c"),
        ("A","b"),
        ("b","d"),
        ("A","end"),
        ("b","end"),
    ].iter()
    .map(|(p1,p2)| (p1.to_string(), p2.to_string()))
    .collect()
}

fn make_graph(input: &[(String,String)]) -> HashMap<&str, Vec<&str>> {
    let mut graph_map: HashMap<&str, Vec<&str>> = HashMap::new(); 
    for (n1,n2) in input {
        graph_map.insert(n1, Vec::new());
        graph_map.insert(n2, Vec::new());
    }
    
    for (n1,n2) in input {
        let n1_neighbors = graph_map.get_mut(&n1 as &str).unwrap();
        n1_neighbors.push(n2);

        let n2_neighbors = graph_map.get_mut(&n2 as &str).unwrap();
        n2_neighbors.push(n1);
    }

    graph_map
}

fn get_first_char(string: &str) -> Option<char> {
    string.chars().nth(0)
}

fn is_available((node,i): (&str,usize)) -> bool {
    let first = get_first_char(node).unwrap();

    first.is_ascii_uppercase()
    || (first.is_ascii_lowercase()
    && i == 1)
}

fn is_covered(node: &str, covered_nodes: &Vec<(&str,usize)>) -> bool {
    let first = get_first_char(node).unwrap();

    first.is_ascii_lowercase()
    && covered_nodes.contains(&(node,1))
}

fn get_paths<'a>(graph: &HashMap<&str, Vec<&'a str>>) -> Vec<Vec<&'a str>> {
    let mut stack = Vec::<(&str,usize,&str,usize)>::new();
    let mut paths = Vec::<Vec<(&str,usize)>>::new();
    let mut current_path = vec![("start", 1)];

    for node in &graph["start"] {
        stack.push(("start", 1, node, 1));
    }

    while !stack.is_empty() {
        let (parent, i, current_node, j) = stack.pop().unwrap();
        while (parent,i) != *current_path.last().unwrap() {
            current_path.pop();
        }

        if is_available((current_node, j)) {
            current_path.push((current_node, j));
        }

        if current_node == "end" {
            paths.push(current_path.clone());
            current_path.pop();
        }
        else {
            let adjacent_nodes = &graph[current_node];
            if adjacent_nodes.into_iter()
                .all(|node| is_covered(node, &current_path)) {
                current_path.pop();
            }
            else {
                for adjacent in adjacent_nodes {
                    let adjacent_count = current_path.iter()
                        .filter(|(node,_)| adjacent == node)
                        .count();
                    if is_available((adjacent, adjacent_count+1)) {
                        stack.push((current_node, j, adjacent, adjacent_count+1));
                    }
                }
            }
        }
    }

    paths.iter()
        .map(|path| path.into_iter()
            .map(|&(node,_)| node)
            .collect())
        .collect()
}

fn is_available_2((node,i): (&str,usize), covered_nodes: &Vec<(&str,usize)>) -> bool {
    let first = get_first_char(node).unwrap();

    let has_no_duplicates =
        covered_nodes.iter()
            .filter(|&(cov_node,_)| 
                get_first_char(cov_node)
                    .unwrap()
                    .is_ascii_lowercase())
            .all(|&(_,j)| j == 1);

    first.is_ascii_uppercase()
    || (first.is_ascii_lowercase()
    && node != "start"
    && (i == 1 && !covered_nodes.contains(&(node,1))
    || i == 2 && has_no_duplicates))
}

fn is_covered_2(node: &str, covered_nodes: &Vec<(&str,usize)>) -> bool {
    let has_duplicate =
        covered_nodes.iter()
            .filter(|&(cov_node,i)| 
                get_first_char(cov_node)
                    .unwrap()
                    .is_ascii_lowercase()
                && *i == 2)
            .count() == 1;

    get_first_char(node).unwrap().is_ascii_lowercase()
    && has_duplicate
    && covered_nodes.contains(&(node,1))
}

fn get_paths_2<'a>(graph: &HashMap<&str, Vec<&'a str>>) -> Vec<Vec<&'a str>> {
    let mut stack = Vec::<(&str,usize,&str,usize)>::new();
    let mut paths = Vec::<Vec<(&str,usize)>>::new();
    let mut current_path = vec![("start", 1)];

    for node in &graph["start"] {
        stack.push(("start", 1, node, 1));
    }

    while !stack.is_empty() {
        let (parent, i, current_node, j) = stack.pop().unwrap();
        while (parent,i) != *current_path.last().unwrap() {
            current_path.pop();
        }

        if is_available_2((current_node, j), &current_path) {
            current_path.push((current_node, j));
        }

        if current_node == "end" {
            paths.push(current_path.clone());
            current_path.pop();
        }
        else {
            let adjacent_nodes = &graph[current_node];
            if adjacent_nodes.into_iter()
                .all(|node| is_covered_2(node, &current_path)) {
                current_path.pop();
            }
            else {
                for adjacent in adjacent_nodes {
                    let adjacent_count = current_path.iter()
                        .filter(|(node,_)| adjacent == node)
                        .count();
                    if is_available_2((adjacent, adjacent_count+1), &current_path) {
                        stack.push((current_node, j, adjacent, adjacent_count+1));
                    }
                }
            }
        }
    }

    paths.iter()
        .map(|path| path.into_iter()
            .map(|&(node,_)| node)
            .collect())
        .collect()
}

fn part1(graph: &HashMap<&str, Vec<&str>>) -> usize {
    let paths = get_paths(graph);
    paths.len()
}

fn part2(graph: &HashMap<&str, Vec<&str>>) -> usize {
    let paths = get_paths_2(graph);

    paths.len()
}

fn main() {
    let file_res = fs::read_to_string("input.txt");
    let input: Vec<(String,String)> = 
        if let Ok(contents) = file_res {
            parse_input(&contents)
        }
        else {
            get_test_input()
        };

    let graph = make_graph(&input);
    println!("{}", part1(&graph));
    println!("{}", part2(&graph));
}
