/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 11/4/19 8:03 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::ast::declaration::{Class as ASTClass, FuncSignature, FunctionArg};
use crate::ast::module::Module;
use crate::lexer::token::Token;
use crate::mir::generator::{MIRGenerator, Res};
use crate::mir::generator::passes::declare_func::create_function;
use crate::mir::nodes::{Class, ClassPrototype};

/// This pass declares all classes.
/// It does not fill them; they are kept empty.
pub fn declare_class_pass(gen: &mut MIRGenerator, module: &mut Module) -> Res<()> {
    for class in module.classes.iter_mut() {
        create_class(gen, class)?;
    }

    Ok(())
}

fn create_class(gen: &mut MIRGenerator, class: &mut ASTClass) -> Res<()> {
    gen.builder.try_reserve_name(&class.name)?;
    gen.builder.add_this_alias(&class.name);

    let this_arg = FunctionArg::this_arg(&class.name);
    let init_fn_name = Token::generic_identifier(format!("{}-internal-init", &class.name.lexeme));
    let init_fn_sig = FuncSignature {
        name: init_fn_name.clone(),
        generics: class.generics.clone(),
        return_type: None,
        parameters: vec![this_arg.clone()],
    };

    // If the AST class has generic parameters, it must be compiled to
    // a class prototype instead of an actual class.
    if let Some(generics) = &class.generics {
        gen.builder.set_generic_types(generics);

        let mut mir_class = ClassPrototype {
            name: Rc::clone(&class.name.lexeme),
            generic_args: gen.builder.generic_types.iter().cloned().collect(),
            ..Default::default()
        };

        let init_fn = create_function(gen, &init_fn_sig)?.right().unwrap();
        mir_class.methods.insert(init_fn_name.lexeme, init_fn);

        // Do all user-defined methods
        for method in class.methods.iter_mut() {
            let method_name = Rc::clone(&method.sig.name.lexeme);
            // Change the method name to $class-$method to prevent name collisions
            method.sig.name.lexeme =
                Rc::new(format!("{}-{}", class.name.lexeme, method.sig.name.lexeme));
            method.sig.parameters.insert(0, this_arg.clone());

            let mir_method = create_function(gen, &method.sig)?.right().unwrap();
            mir_class.methods.insert(method_name, mir_method);
        }

        gen.builder.generic_types.clear();
    } else {
        let mut mir_class = Class {
            name: Rc::clone(&class.name.lexeme),
            ..Default::default()
        };

        let init_fn = create_function(gen, &init_fn_sig)?.left().unwrap();
        mir_class.methods.insert(init_fn_name.lexeme, init_fn);

        // Do all user-defined methods
        for method in class.methods.iter_mut() {
            let method_name = Rc::clone(&method.sig.name.lexeme);
            // Change the method name to $class-$method to prevent name collisions
            method.sig.name.lexeme =
                Rc::new(format!("{}-{}", class.name.lexeme, method.sig.name.lexeme));
            method.sig.parameters.insert(0, this_arg.clone());

            let mir_method = create_function(gen, &method.sig)?.left().unwrap();
            mir_class.methods.insert(method_name, mir_method);
        }
    }

    gen.builder.remove_this_alias();
    Ok(())
}
