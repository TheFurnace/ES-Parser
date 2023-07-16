use esparser::esparser::parse;

fn main() {
    let target = include_str!("../assets/weapons.txt");
    let result = parse(target);

    print!("File parsed, length: {}, value: {:#?}", result.len(), result)
}