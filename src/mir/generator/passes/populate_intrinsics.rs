/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/15/19 4:19 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::error::Res;
use crate::mir::generator::intrinsics::INTRINSICS;
use crate::mir::generator::passes::{ModulePass, PassType};
use crate::mir::{MModule, MutRc};

/// This pass populates the intrinsics struct.
pub struct PopulateIntrinsics();

impl ModulePass for PopulateIntrinsics {
    fn get_type(&self) -> PassType {
        PassType::GlobalInspect
    }

    fn run_inspect(&mut self, modules: &Vec<MutRc<MModule>>) -> Res<()> {
        for module in modules {
            let module = module.borrow();
            if **module.path.0[0] == *"std" && **module.path.0[1] == *"ops" {
                // This is the std/ops module, containing all operator interfaces
                INTRINSICS.with(|i| i.borrow_mut().fill_ops_table(module))
            }
        }
        Ok(())
    }
}
