/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/16/19 9:26 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::error::Errors;
use crate::mir::{MModule, MutRc};
use crate::mir::generator::intrinsics::INTRINSICS;
use crate::mir::generator::passes::{ModulePass, PassType};

/// This pass populates the intrinsics struct.
pub struct PopulateIntrinsics();

impl ModulePass for PopulateIntrinsics {
    fn get_type(&self) -> PassType {
        PassType::Globally
    }

    fn run_globally(&self, modules: &[MutRc<MModule>]) -> Result<(), Vec<Errors>> {
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
