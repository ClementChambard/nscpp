// use crate::parsing::{CharType, FloatType, IntegerType};
//
// pub enum IdentType {
//     Type(usize), // points to type
//     Var(usize),  // points to type
//     Fun()
// }
//
// pub struct Identifier {
//     ident: IdentType,
//     name: String,
// }
//
// pub struct Class {}
//
// pub struct Union {}
//
// // TODO: templates
// enum Type {
//     Integer(IntegerType),
//     Float(FloatType),
//     Char(CharType),
//     Bool,
//     Nullptr,
//     Void,
//     Alias(usize),           // points to type it aliases.
//     Enum(IntegerType),      // underlying integer type
//     EnumClass(IntegerType), // underlying integer type
//     Class(Class),           // either struct or class
//     Union(Union),
// }
//
// pub struct Scope {}
//
// pub struct GlobalScope {
//     namespace: String,
//     scope: Scope,
// }
//
// pub struct Context {
//     known_types: Vec<Type>,
//     global_scopes: Vec<GlobalScope>,
//     local_scope_stack: Vec<Scope>,
// }

pub type NamespaceName = Vec<String>;

#[derive(Debug, Clone, PartialEq)]
pub enum NameType {
    Namespace(usize), // idx of namespace inside the sub_namespaces array.
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name {
    name: String,
    ty: NameType,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Namespace {
    name: String,
    sub_namespaces: Vec<Namespace>,
    names: Vec<Name>,
}

impl Namespace {
    pub fn lookup_name(&self, s: &str) -> Option<&Name> {
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    names: Vec<Name>,
}

impl Scope {
    pub fn lookup_name(&self, s: &str) -> Option<&Name> {
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    global_namespace: Namespace,
    scopes: Vec<Scope>,
    current_namespace: NamespaceName,
}

impl Context {
    pub fn new() -> Self {
        Self {
            global_namespace: Namespace::default(),
            scopes: Vec::new(),
            current_namespace: NamespaceName::new(),
        }
    }

    fn get_namespace(&self, namespace: &NamespaceName) -> Option<&Namespace> {
        if namespace.is_empty() {
            return Some(&self.global_namespace);
        }
        None
    }

    pub fn lookup_name(&self, namespace: Option<&NamespaceName>, s: &str) -> Option<&Name> {
        if let Some(ns) = namespace {
            // TODO: Error if unknown namespace ?
            self.get_namespace(ns)?.lookup_name(s)
        } else {
            for i in (0..self.scopes.len()).rev() {
                if let Some(n) = self.scopes.get(i).unwrap().lookup_name(s) {
                    return Some(n);
                }
            }
            // - check current namespace and its parents
            None
        }
    }
}
