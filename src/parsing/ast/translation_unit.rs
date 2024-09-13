use crate::{parsing::{
    ast::{parse_util::parse_elt_list, Decl},
    context::Context,
    Token,
}, util::{color, ast_printing}};

pub struct TranslationUnit {
    decls: Vec<Decl>,
    context: Context,
}

impl TranslationUnit {
    pub fn parse(toks: &[Token]) -> Self {
        if toks.is_empty() {
            panic!("ERROR: NO TOKENS TO PARSE");
        }
        let mut context = Context::new();
        let decls = parse_elt_list(toks, &mut context, Decl::parse).unwrap().0;
        Self { decls, context }
    }

    pub fn print(&self) {
        use ast_printing::AstPrint;
        println!("{}", self.print_ast("", ""));
    }
}

impl ast_printing::AstPrint for TranslationUnit {
    fn print_ast(&self, fstr: &str, lstr: &str) -> String {
        let mut out = ast_printing::start_str(color::GREEN, "TranslationUnitDecl", fstr);
        out.push('\n');
        out.push_str(&ast_printing::print_ast_list(&self.decls, lstr));
        out
    }
}
