pub mod reader;
pub mod scanner;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scanner() {
        let mut scnr = scanner::Scanner::new("let x = function () {};".chars());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
        println!("{:?}", scnr.next(scanner::LexGoal::RegExp).unwrap());
    }
}
