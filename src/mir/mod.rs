/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 9/11/19, 7:43 PM.
 * This file is under the GPL3 license. See LICENSE in the root directory of this repository for details.
 */

use nodes::{MIRStruct, MIRVariable};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::ModulePath;

pub mod generator;
pub mod nodes;

type MutRc<T> = Rc<RefCell<T>>;

fn mutrc_new<T>(value: T) -> MutRc<T> {
    Rc::new(RefCell::new(value))
}

/// A struct produced by a generator. It contains the full MIR representation of a file/module.
#[derive(Debug, Default)]
pub struct MIRModule {
    pub path: Rc<ModulePath>,
    pub types: HashMap<Rc<String>, MutRc<MIRStruct>>,
    pub functions: HashMap<Rc<String>, Rc<MIRVariable>>,
}

impl MIRModule {
    pub fn new(path: Rc<ModulePath>) -> MIRModule {
        Self {
            path,
            ..Default::default()
        }
    }
}
