/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 10/12/19 5:46 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::fmt::{Display, Error, Formatter};
use std::rc::Rc;

use either::Either;

use crate::ast::declaration::Function;
use crate::ast::expression::Expression;
use crate::mir::nodes::MIRArray;

/// An enum containing all literals possible in Gelix.
#[derive(Debug, Clone)]
pub enum Literal {
    Any,
    None,
    Bool(bool),

    // The Rust representation of these integers can be unsigned
    // since literals themselves are always unsigned.
    // (A negative literal is just a unary negated literal)
    I8(u8),
    I16(u8),
    I32(u16),
    I64(u32),

    F32(f32),
    F64(f64),

    Char(char),
    String(Rc<String>),

    Array(Either<Rc<Vec<Expression>>, MIRArray>),

    Closure(Rc<Function>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Literal::Any => write!(f, "Any"),
            Literal::None => write!(f, "None"),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::I8(num) => write!(f, "{}i8", num),
            Literal::I16(num) => write!(f, "{}i16", num),
            Literal::I32(num) => write!(f, "{}i32", num),
            Literal::I64(num) => write!(f, "{}", num),
            Literal::F32(num) => write!(f, "{}f", num),
            Literal::F64(num) => write!(f, "{}d", num),
            Literal::Char(ch) => write!(f, "'{}'", ch),
            Literal::String(st) => write!(f, "\"{}\"", st),
            Literal::Array(_) => write!(f, "<array literal>"),
            Literal::Closure(_) => write!(f, "<closure>"),
        }
    }
}