mod errors;
mod file_opening;
mod parsing;
mod preprocessor;
mod util;

pub use errors::ErrorBuilder;

fn main() {
    let mut args = std::env::args();
    let _exe_name = args.next().unwrap();
    let filename = args.next().unwrap_or(String::from("test.cpp"));
    let tokens = file_opening::open_file(&filename);
    let tokens = preprocessor::preprocess_file(&filename, tokens);
    let tokens = parsing::translate_tokens(tokens);
    // for t in tokens {
    //     println!("{:<20}: {:?}", format!("{}", t.loc), t.tok);
    // }
    let tu = parsing::ast::TranslationUnit::parse(&tokens);
    tu.print();
}
