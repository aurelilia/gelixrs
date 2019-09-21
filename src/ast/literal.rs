/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 9/21/19 4:30 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use either::Either;

use crate::ast::declaration::Function;
use crate::ast::expression::Expression;
use crate::mir::nodes::{MIRArray, MIRExpression, MIRType};

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
