use std::fs;

const RAW_TEST_INPUT: &str =
    // "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
    edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
    fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
    fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
    aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
    dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
    egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
    gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";


struct Entry {
    signals: Vec<Vec<usize>>,
    outputs: Vec<Vec<usize>>
}

fn get_input(raw_input: &str) -> Vec<Entry> {
    let raw_entries: Vec<&str> = raw_input.split("\n").collect();

    let mut entries = Vec::new();
    for raw_entry in raw_entries {
        let parts: Vec<&str> = raw_entry.split(" | ").collect();
        let signals: Vec<&str> = parts[0].split(" ").collect();
        let outputs: Vec<&str> = parts[1].split(" ").collect();

        let mut signals: Vec<Vec<usize>> = signals.into_iter()
            .map(|signal| signal.chars().map(|c| char_to_int(c)).collect())
            .collect();

        let mut outputs: Vec<Vec<usize>> = outputs.into_iter()
            .map(|output| output.chars().map(|c| char_to_int(c)).collect())
            .collect();

        for signal in signals.iter_mut() {
            signal.sort();
        }

        for output in outputs.iter_mut() {
            output.sort();
        }

        entries.push(Entry { signals, outputs });
    }

    entries
}

fn part1(entries: &[Entry]) -> usize {
    let mut count = 0;
    for entry in entries {
        for output in &entry.outputs {
            if output.len() == 2
            || output.len() == 3
            || output.len() == 4
            || output.len() == 7 {
                count += 1;
            }
        }
    }

    count
}

fn char_to_int(c: char) -> usize {
    match c {
        'a' => 0,
        'b' => 1,
        'c' => 2,
        'd' => 3,
        'e' => 4,
        'f' => 5,
        'g' => 6,
        _ => usize::MAX
    } 
}

fn find_eg(entry: &Entry) -> (usize, usize) {
    let mut possible1: Option<[usize; 2]> = None;
    let mut possible7: Option<[usize; 3]> = None;
    let mut possible4: Option<[usize; 4]> = None;
    let mut possible8: Option<[usize; 7]> = None;

    // find unique length signals
    for signal in &entry.signals {
        if signal.len() == 2 
        && possible1.is_none() {
            possible1 = Some([signal[0], signal[1]]);
        }
        else if signal.len() == 3
        && possible7.is_none() {
            possible7 = Some([signal[0], signal[1], signal[2]]);
        }
        else if signal.len() == 4
        && possible4.is_none() {
            possible4 = Some([signal[0], signal[1], signal[2], signal[3]]);
        }
        else if signal.len() == 7
        && possible8.is_none() {
            possible8 = Some([signal[0], signal[1], signal[2], signal[3], 
                signal[4], signal[5], signal[6]]);
        }
    }

    // find e,g
    let mut map_eg: Vec<usize> = Vec::new();
    for c in &possible8.unwrap() {
        if !possible4.unwrap().contains(c) 
        && !possible7.unwrap().contains(c) {
            map_eg.push(*c);
        }
    }

    let count_first_eg = entry.signals.iter()
        .filter(|signal| signal.contains(&map_eg[0]))
        .count();

    if count_first_eg == 4 {
        (map_eg[0], map_eg[1])
    }
    else {
        (map_eg[1], map_eg[0])
    }
}

fn find_bf(entry: &Entry) -> (usize, usize) {
    let mut map_b: Option<usize> = None;
    let mut map_f: Option<usize> = None;
    
    for c in 0..=9 {
        let count = entry.signals.iter()
            .filter(|signal| signal.contains(&c))
            .count();
        if count == 6 {
            map_b = Some(c);
        }
        else if count == 9 {
            map_f = Some(c);
        }
    }

    (map_b.unwrap(), map_f.unwrap())
}

fn find_dg(entry: &Entry) -> (usize, usize) {
    let mut map_dg: Vec<usize> = Vec::new();
    
    for c in 0..=9 {
        let count = entry.signals.iter()
            .filter(|signal| signal.contains(&c))
            .count();
        if count == 7 {
            map_dg.push(c);
        }
    }

    (map_dg[0], map_dg[1])
}

fn find_ac(entry: &Entry) -> (usize, usize) {
    let mut map_ac: Vec<usize> = Vec::new();    
    for c in 0..=9 {
        let count = entry.signals.iter()
            .filter(|signal| signal.contains(&c))
            .count();
        if count == 8 {
            map_ac.push(c);
        }
    }

    let mut unique_signals = Vec::new();

    // find unique length signals
    for signal in &entry.signals {
        if signal.len() == 2 {
            unique_signals.push(signal);
        }
        else if signal.len() == 3 {
            unique_signals.push(signal);
        }
        else if signal.len() == 4{
            unique_signals.push(signal);
        }
        else if signal.len() == 7 {
            unique_signals.push(signal);
        }
    }

    let count = unique_signals.iter()
            .filter(|signal| signal.contains(&map_ac[0]))
            .count();

    if count == 2 {
        (map_ac[0], map_ac[1])
    }
    else {
        (map_ac[1], map_ac[0])
    }
}

fn get_map(entry: &Entry) -> Vec<usize> {
    /*
    counts of letters:
    a: 8 <- from unique signals
    b: 6 
    c: 8 <- from unique signals
    d: 7
    e: 4 <- unique signal lengths
    f: 9
    g: 7 <- unique signal lengths
    */
    let mut map: [Option<usize>; 7] = [None; 7];

    let (map_e, map_g) = find_eg(&entry);
    map[map_e] = Some(char_to_int('e'));
    map[map_g] = Some(char_to_int('g'));

    let (map_b, map_f) = find_bf(&entry);
    map[map_b] = Some(char_to_int('b'));
    map[map_f] = Some(char_to_int('f'));

    let (map_d1, map_d2) = find_dg(&entry);
    if map_d1 == map_g {
        map[map_d2] = Some(char_to_int('d'));   
    }
    else {
        map[map_d1] = Some(char_to_int('d'));
    }

    let (map_a, map_c) = find_ac(&entry);
    map[map_a] = Some(char_to_int('a'));
    map[map_c] = Some(char_to_int('c'));

    println!("map: {:?}", map);

    map.iter().flatten().map(|m| *m).collect()
}

fn display_to_digit(map: &[usize], display: &[usize]) -> usize {
    let conversion: [Vec<usize>; 10] = [
        // 0
        vec![0, 1, 2, 4, 5, 6],
        // 1
        vec![2, 5],
        // 2
        vec![0, 2, 3, 4, 6],
        // 3
        vec![0, 2, 3, 5, 6],
        // 4
        vec![1, 2, 3, 5],
        // 5
        vec![0, 1, 3, 5, 6],
        // 6
        vec![0, 1, 3, 4, 5, 6],
        // 7
        vec![0, 2, 5],
        // 8
        vec![0, 1, 2, 3, 4, 5, 6],
        // 9
        vec![0, 1, 2, 3, 5, 6]
    ];

    println!("display: {:?}", display);

    let mut mapped_display: Vec<usize> = display.iter()
        .map(|&c| map[c])
        .collect();

    mapped_display.sort();

    let mut converted_digit: Option<usize> = None;
    println!("mapped display: {:?}", mapped_display);
    for (digit,digit_conv) in conversion.iter().enumerate() {
        println!("digit convert: {:?}", digit_conv);
        if mapped_display == digit_conv.clone() {
            converted_digit = Some(digit);
            break;
        }
    }

    converted_digit.unwrap()
}

fn part2(entries: &[Entry]) -> usize {
    let mut sum = 0;
    for entry in entries {
        let map = get_map(&entry);

        let mut number = 0;
        for output in &entry.outputs {
            println!("output: {:?}", output);
            let digit = display_to_digit(&map, output);
            println!("digit: {}", number);
            number = number * 10 + digit;
        }

        println!("number: {}", number);
        sum += number; 
    }

    sum
}

fn main() {
    let file_result = fs::read_to_string("input.txt");
    let entries = 
        if let Ok(contents) = file_result {
            get_input(&contents)
        }
        else {
            get_input(&RAW_TEST_INPUT)
        };

    println!("{}", part1(&entries));
    println!("{}", part2(&entries));
}
