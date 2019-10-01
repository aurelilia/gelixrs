/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 10/2/19 1:22 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::ast::declaration::{ASTType, FunctionArg, Interface};
use crate::ast::module::Module;
use crate::lexer::token::Token;
use crate::mir::generator::{MIRGenerator, Res};
use crate::mir::generator::passes::declare_func::create_function;

/// This pass declares all interfaces.
pub fn declare_interface_pass(gen: &mut MIRGenerator, module: &mut Module) -> Res<()> {
    for interface in module.interfaces.iter_mut() {
        create_interface(gen, interface)?
    }

    Ok(())
}

fn create_interface(gen: &mut MIRGenerator, interface: &mut Interface) -> Res<()> {
    let mir_iface = gen
        .builder
        .create_interface(&interface.name.lexeme)
        .ok_or(gen.error(
            &interface.name,
            &interface.name,
            "Interface with the same name already defined.",
        ))?;
    let mut mir_iface = mir_iface.borrow_mut();

    let this_arg = FunctionArg::this_arg(&interface.name);
    for method in interface.methods.iter_mut() {
        let old_name = Rc::clone(&method.sig.name.lexeme);
        method.sig.name.lexeme = Rc::new(format!(
            "{}-{}",
            interface.name.lexeme, method.sig.name.lexeme
        ));
        method.sig.parameters.insert(0, this_arg.clone());

        let mir_method = create_function(gen, &method.sig)?;
        mir_iface.methods.insert(old_name, Rc::clone(&mir_method));
        mir_iface.methods_order.push(mir_method);
    }

    Ok(())
}
