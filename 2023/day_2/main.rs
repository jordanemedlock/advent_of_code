use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::env;
use std::cmp::max;

fn parse_colors(color_str: &str) -> (i32, i32, i32) {
    let (n, color) = color_str.split_once(" ").unwrap();
    let num: i32 = n.parse().unwrap();
    if color == "red" {
        return (num, 0, 0);
    } else if color == "green" {
        return (0,num, 0);
    } else if color == "blue" {
        return (0,0,num);
    }
    return (0,0,0);
}

fn parse_round(round_str: &str) -> (i32, i32, i32) {
    let colors: (i32, i32, i32) = round_str.split(",")
        .map(|e| parse_colors(e.trim()))
        .fold((0,0,0), |acc, e| (acc.0 + e.0, acc.1 + e.1, acc.2 + e.2));
    return colors;
}

fn count_game(game_line: &str) -> (i32, (i32, i32, i32)) {
    if let Some((game_num, rest)) = game_line.trim_start_matches("Game ").split_once(":") {
        return (
            game_num.parse().unwrap(), 
            rest.split(";")
                .map(|e| parse_round(e.trim()))
                .fold((0,0,0), |acc, e| (max(acc.0, e.0), max(acc.1, e.1), max(acc.2, e.2)))
        );
    }
    return (0, (0,0,0));
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // File hosts.txt must exist in the current path
    let games: Vec<(i32, (i32, i32, i32))> = include_str!("./input.txt")
        .split("\n")
        .map(|e| count_game(e))
        .collect();

    if args.len() == 2 && args[1] == "--part-1" {
        let count: i32 = games.iter()
            .filter(|(_id, nums)| nums.0 <= 12 && nums.1 <= 13 && nums.2 <= 14)
            .map(|(id, _)| id)
            .sum();
        println!("{}", count);
    } else if args.len() == 2 && args[1] == "--part-2" {
        let sum_of_powers: i32 = games.iter()
            .map(|(id, (r, g, b))| r*g*b)
            .sum();
        println!("{}", sum_of_powers);
    } else {
        println!("Advent of code: Day 2");
        println!("Usage: cargo run -- (--part-1|--part-2)");
    }

}