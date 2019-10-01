/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 10/1/19 4:58 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::ast::module::Module;
use crate::mir::generator::Res;

pub(super) mod declare_class;
pub(super) mod declare_func;
pub(super) mod declare_interface;
pub(super) mod fill_class;
pub(super) mod iface_impl;
pub(super) mod import;

/// A trait for passes that modify the AST or the generator before MIR creation.
pub(super) trait PreMIRPass {
    fn run(self, module: &mut Module) -> Res<()>;
}
