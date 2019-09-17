/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 9/12/19 10:53 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::ast::declaration::{Class, Enum, FuncSignature, Function};
use crate::ModulePath;
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct Module {
    pub path: Rc<ModulePath>,

    pub classes: Vec<Class>,
    pub enums: Vec<Enum>,
    pub ext_functions: Vec<FuncSignature>,
    pub functions: Vec<Function>,
    pub imports: Vec<Import>,
}

impl Module {
    pub fn new(path: &ModulePath) -> Self {
        Self {
            path: Rc::new(path.clone()),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: ModulePath,
    pub symbol: Rc<String>,
}
