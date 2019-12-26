/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/26/19 3:29 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::ast::Module;
use crate::error::Errors;
use crate::mir::generator::builder::Context;
use crate::mir::generator::passes::PreMIRPass;
use crate::mir::nodes::{Class, Interface, Type};
use crate::mir::{mutrc_new, MModule, MutRc};

/// This pass defines all types inside the module; currently classes and interfaces.
/// It only creates a stub MIR definition and inserts it as a type;
/// nothing is filled or created.
pub struct DeclareTypes();

impl PreMIRPass for DeclareTypes {
    fn run(
        &mut self,
        ast: &mut Module,
        module: MutRc<MModule>,
        _modules: &[MutRc<MModule>],
    ) -> Result<(), Errors> {
        let mut module = module.borrow_mut();
        let mut errs = Vec::new();

        for class in ast.classes.drain(..) {
            let name = class.name.clone();
            module
                .try_reserve_name(&name)
                .map_err(|e| errs.push(e))
                .ok();

            module
                .types
                .insert(Rc::clone(&name.lexeme), Type::Class(Class::from_ast(class, Context::default())));
        }

        for iface in ast.interfaces.drain(..) {
            let name = iface.name.clone();
            module
                .try_reserve_name(&name)
                .map_err(|e| errs.push(e))
                .ok();

            module
                .types
                .insert(Rc::clone(&name.lexeme), Type::Interface(Interface::from_ast(iface, None, Context::default())));
        }

        if errs.is_empty() {
            Ok(())
        } else {
            Err(Errors(errs, Rc::clone(&module.src)))
        }
    }
}
