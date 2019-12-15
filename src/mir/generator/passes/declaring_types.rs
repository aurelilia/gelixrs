/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/15/19 2:07 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::error::Res;
use crate::mir::{MModule, MutRc};
use crate::mir::generator::passes::ModulePass;

/// This pass defines all types inside the module; currently classes and interfaces.
/// It only creates a stub MIR definition and inserts it as a type;
/// nothing is filled or created.
pub struct DeclareTypes();

impl ModulePass for DeclareTypes {
    fn run(&mut self, module: MutRc<MModule>) -> Res<()> {
        let mut module = module.borrow_mut();

        for class in module.ast.classes.iter_mut() {
            // TODO
        }

        for iface in module.ast.interfaces.iter_mut() {
            // TODO
        }

        Ok(())
    }
}