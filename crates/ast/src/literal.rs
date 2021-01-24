/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/27/19 6:50 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::Literal;
use smol_str::SmolStr;
use syntax::kind::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralType {
    Null,
    True,
    False,
    Int,
    Float,
    String,
}

impl Literal {
    #[allow(unused)]
    pub fn get(&self) -> (SmolStr, LiteralType) {
        let child = self.cst.children_with_tokens().next().unwrap();
        let token = child.as_token().unwrap();
        let kind = match token.kind() {
            SyntaxKind::Null => LiteralType::Null,
            SyntaxKind::False => LiteralType::False,
            SyntaxKind::True => LiteralType::True,
            SyntaxKind::Int => LiteralType::Int,
            SyntaxKind::Float => LiteralType::Float,
            SyntaxKind::String => LiteralType::String,
            _ => panic!("AST encountered unknown CST literal"),
        };
        (token.text().clone(), kind)
    }
}
