/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/15/19 2:07 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

//! This module contains things related to intrinsics: things that
//! bridge the gap between the compiler and the language.
//! An example would be the translation of operator
//! overloading interfaces into actually changing the behavior
//! of the expression.

use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, Res};
use crate::lexer::token::TType;
use crate::mir::{MModule, MutRc};
use crate::mir::nodes::{Prototype, Variable};

thread_local! {
    pub static INTRINSICS: RefCell<Intrinsics> = RefCell::new(Intrinsics::default());
}

/// Contains all data structures that require some sort of special treatment.
#[derive(Default)]
pub struct Intrinsics {
    ops: HashMap<TType, MutRc<dyn Prototype>>,
    pub main_fn: Option<Rc<Variable>>,
}

impl Intrinsics {
    pub fn get_op_iface(&self, ty: TType) -> MutRc<dyn Prototype> {
        Rc::clone(&self.ops[&ty])
    }

    pub fn populate(&mut self, modules: &Vec<MutRc<MModule>>) {
        for module in modules.iter() {
            let mut module = module.borrow_mut();
            if **module.path.0[0] == *"std" && **module.path.0[1] == *"ops" {
                // This is the std/ops module, containing all operator interfaces
                self.fill_ops_table(module)
            }
        }
    }

    fn fill_ops_table(&mut self, module: RefMut<MModule>) {
        for (name, iface) in module.protos.iter() {
            let iface = Rc::clone(iface);
            match &name[..] {
                "Add" => self.ops.insert(TType::Plus, iface),
                "Sub" => self.ops.insert(TType::Minus, iface),
                "Mul" => self.ops.insert(TType::Star, iface),
                "Div" => self.ops.insert(TType::Slash, iface),
                "Equal" => {
                    self.ops.insert(TType::EqualEqual, Rc::clone(&iface));
                    self.ops.insert(TType::BangEqual, iface)
                }
                _ => None,
            };
        }
    }

    pub fn set_main_fn(&mut self, func: &Rc<Variable>) -> Option<()> {
        match self.main_fn {
            Some(_) => None,
            None => {
                self.main_fn = Some(Rc::clone(func));
                Some(())
            }
        }
    }

    pub fn validate(&mut self) -> Res<()> {
        if self.main_fn.is_none() {
            return Err(Error {
                line: 0,
                start: 0,
                len: 0,
                producer: "MIR",
                message: "Could not find main function.".to_string(),
                module: Rc::new(Default::default()),
            });
        }
        Ok(())
    }

    pub fn reset(&mut self) {
        self.ops.clear();
        self.main_fn = None;
    }
}
