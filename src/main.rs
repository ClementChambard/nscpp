extern crate unicode_xid;
mod file_opening;

// TODO:
// HeaderName
// Raw string litteral
// Escape sequences
// Multichar character litteral
//   
// NEXT

fn main() {
    let tokens = file_opening::open_file("test.cpp");
    for t in tokens {
        println!("{}: {:?}", t.loc, t.tok);
    }
}
