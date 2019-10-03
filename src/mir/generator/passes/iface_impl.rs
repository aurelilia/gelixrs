/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 10/3/19 4:14 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::ast::declaration::{ASTType, FuncSignature, FunctionArg, IFaceImpl};
use crate::ast::module::Module;
use crate::lexer::token::Token;
use crate::mir::generator::{MIRGenerator, Res};
use crate::mir::generator::passes::declare_func::create_function;
use crate::mir::generator::passes::THIS_CONST;
use crate::mir::nodes::{MIRFunction, MIRIFaceMethod, MIRType, MIRVariable};

/// This pass checks and defines all interface impl blocks.
pub fn iface_impl_pass(gen: &mut MIRGenerator, list: &mut Module) -> Res<()> {
    for iface in list.iface_impls.iter_mut() {
        iface_impl(gen, iface)?;
    }
    Ok(())
}

fn iface_impl(gen: &mut MIRGenerator, iface_impl: &mut IFaceImpl) -> Res<()> {
    gen.builder.add_alias(&THIS_CONST.with(|c| c.clone()), &ASTType::Token(iface_impl.class.clone()));
    let class = gen
        .builder
        .find_class(&iface_impl.class.lexeme)
        .ok_or_else(|| gen.error(&iface_impl.class, &iface_impl.class, "Unknown class."))?;
    let mut class = class.borrow_mut();

    let iface_cell = gen
        .builder
        .find_interface(&iface_impl.iface.lexeme)
        .ok_or_else(|| gen.error(&iface_impl.iface, &iface_impl.iface, "Unknown interface."))?;
    let iface = iface_cell.borrow();

    if iface_impl.iface_generics.len() != iface.generics.len() {
        return Err(gen.error(
            &iface_impl.iface,
            &iface_impl.iface,
            &format!("Wrong amount of interface generic parameters (expected {}; got {})", iface.generics.len(), iface_impl.iface_generics.len()),
        ))
    }
    for (g_impl, g_iface) in iface_impl.iface_generics.iter().zip(iface.generics.iter()) {
        gen.builder.add_alias(g_iface, &ASTType::Token(g_impl.clone()));
    }

    for method in iface_impl.methods.iter_mut() {
        let iface_method = match iface.methods.get(&method.sig.name.lexeme) {
            Some(method) => method,
            None => return Err(gen.error(
                &method.sig.name,
                &method.sig.name,
                "Method is not defined in interface.",
            ))?
        };

        let this_arg = FunctionArg::this_arg(&iface_impl.class);

        let old_name = Rc::clone(&method.sig.name.lexeme);
        method.sig.name.lexeme = Rc::new(format!(
            "{}-{}",
            iface_impl.class.lexeme, method.sig.name.lexeme
        ));
        method.sig.parameters.insert(0, this_arg);

        let mir_method = create_function(gen, &method.sig)?;
        class.methods.insert(old_name, Rc::clone(&mir_method));

        let mir_method = if let MIRType::Function(method) = &mir_method._type { method } else { panic!() };
        let mir_method = mir_method.borrow();

        if mir_method.ret_type != gen.builder.translate_generic(&iface_method.ret_type) {
            let tok = method.sig.return_type.as_ref().map(|t| t.get_token()).flatten();
            return Err(MIRGenerator::anon_err(gen, tok, "Incorrect return type on interface method."))
        }

        for (i, (method_param, iface_param)) in mir_method.parameters.iter().zip(iface_method.parameters.iter()).enumerate() {
            if method_param._type != gen.builder.translate_generic(iface_param) {
                let tok = &method.sig.parameters[i].name;
                return Err(MIRGenerator::error(gen, tok, tok, "Incorrect parameter type on interface method."))
            }
        }
    }

    if iface.methods.len() > iface_impl.methods.len() {
        Err(gen.error(
            &iface_impl.iface,
            &iface_impl.iface,
            "Missing methods in interface impl.",
        ))
    } else {
        class.interfaces.push(Rc::clone(&iface_cell));

        gen.builder.remove_alias(&THIS_CONST.with(|c| c.clone()));
        for g_iface in iface.generics.iter() {
            gen.builder.remove_alias(g_iface);
        }

        Ok(())
    }
}
