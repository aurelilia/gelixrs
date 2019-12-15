/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/15/19 3:36 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::error::{Error, Res};
use crate::mir::{MModule, MutRc};
use crate::mir::generator::intrinsics::INTRINSICS;
use crate::mir::generator::passes::{ModulePass, PassType};

/// This pass validates the intrinsics.
pub struct ValidateIntrinsics();

impl ModulePass for ValidateIntrinsics {
    fn get_type(&self) -> PassType { PassType::GlobalInspect }

    fn run_inspect(&mut self, modules_: &Vec<MutRc<MModule>>) -> Res<()> {
        INTRINSICS.with(|i| i.borrow_mut().validate())
    }
}