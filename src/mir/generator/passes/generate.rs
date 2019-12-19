/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/19/19 3:28 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::error::Res;
use crate::mir::generator::passes::{ModulePass, PassType};
use crate::mir::generator::MIRGenerator;
use crate::mir::nodes::Type;

/// This pass generates the bodies of all functions and methods.
pub struct Generate();

impl ModulePass for Generate {
    fn get_type(&self) -> PassType {
        PassType::Type
    }

    fn run_type(&self, gen: &mut MIRGenerator, ty: Type) -> Res<()> {
        ty.context().map(|c| gen.builder.context = c);
        match ty {
            Type::Function(func) => {
                let ast = func.borrow().ast.as_ref().cloned();
                if let Some(ast) = ast {
                    gen.generate_function(&ast, None)?;
                }
            }

            Type::Class(class) => {
                let ast = Rc::clone(&class.borrow().ast);
                gen.generate_constructors(&ast)?;

                for method in ast.methods.iter() {
                    let mir = &class.borrow().methods[&method.sig.name.lexeme];
                    gen.generate_function(method, Some(mir.type_.as_function()))?;
                }
            }

            Type::Interface(_iface) => (),

            _ => panic!("Primitive type in module!"),
        }
        Ok(())
    }
}
