/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 11/4/19 8:03 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::ast::declaration::{Function, FunctionArg, IFaceImpl, Type as ASTType};
use crate::ast::module::Module;
use crate::lexer::token::Token;
use crate::mir::{MutRc, ToMIRResult};
use crate::mir::generator::{MIRGenerator, Res};
use crate::mir::generator::passes::declare_func::create_function;
use crate::mir::nodes::{IFaceMethod, Interface, InterfacePrototype, Type, Variable};
use crate::option::Flatten;

/// This pass checks and defines all interface impl blocks.
pub fn iface_impl_pass(gen: &mut MIRGenerator, list: &mut Module) -> Res<()> {
    for iface in list.iface_impls.iter_mut() {
        iface_impl(gen, iface)?;
    }
    Ok(())
}

fn iface_impl(gen: &mut MIRGenerator, iface_impl: &mut IFaceImpl) -> Res<()> {
    gen.builder.add_this_alias(&iface_impl.class);
    iface_impl.class_generics.as_ref().map(|g| gen.builder.set_generic_types(&g));

    let class = gen
        .builder
        .find_class_or_proto(&iface_impl.class.lexeme)
        .or_err(gen, &iface_impl.class, "Unknown class")?;

    let iface = gen
        .builder
        .find_iface_or_proto(&iface_impl.iface.lexeme)
        .or_err(gen, &iface_impl.iface, "Unknown interface.")?
        .map_left(|l| Ok(l))
        .left_or_else(|proto| iface_from_proto(gen, proto, iface_impl.iface_generics.unwrap_or_else(|| vec![]), &iface_impl.iface))?;

    for method in iface_impl.methods.iter_mut() {
        let iface_method = iface.methods.get(&method.sig.name.lexeme).or_err(
            gen,
            &method.sig.name,
            "Method is not defined in interface.",
        )?;
        let this_arg = FunctionArg::this_arg(&iface_impl.class);

        let old_name = Rc::clone(&method.sig.name.lexeme);
        method.sig.name.lexeme = Rc::new(format!(
            "{}-{}",
            iface_impl.class.lexeme, method.sig.name.lexeme
        ));
        method.sig.parameters.insert(0, this_arg);

        let mir_method = create_function(gen, &method.sig)?;
        class.borrow_mut().methods.insert(old_name, Rc::clone(&mir_method));

        check_equal_signature(gen, method, mir_method, iface_method)?;
    }

    if iface.methods.len() > iface_impl.methods.len() {
        Err(gen.error(
            &iface_impl.iface,
            &iface_impl.iface,
            "Missing methods in interface impl.",
        ))
    } else {
        class.borrow_mut().interfaces.insert(Rc::clone(&iface.name), Rc::clone(&iface));

        gen.builder.remove_this_alias();
        for g_iface in iface.generics.iter() {
            gen.builder.remove_alias(g_iface);
        }

        Ok(())
    }
}

fn iface_from_proto(gen: &mut MIRGenerator, proto: MutRc<InterfacePrototype>, args: Vec<Token>, error_tok: &Token) -> Res<MutRc<Interface>> {
    let args = args.iter().map(|tok| gen.builder.find_type(&ASTType::Ident(tok.clone()))).collect()?;
    let iface = proto.borrow_mut().build(args);
    let iface = iface.map_err(|msg| gen.error(error_tok, error_tok, &msg))?;
    Ok(iface)
}

/// Registers all generic parameter aliases; also does checking to ensure they are valid.
fn add_generic_aliases(
    gen: &mut MIRGenerator,
    iface: &Vec<Rc<String>>,
    impl_: &Vec<Token>,
    iface_tok: &Token,
) -> Res<()> {
    if iface.len() != impl_.len() {
        return Err(gen.error(
            iface_tok,
            iface_tok,
            &format!(
                "Wrong amount of interface generic parameters (expected {}; got {})",
                iface.len(),
                impl_.len()
            ),
        ));
    }
    for (g_impl, g_iface) in impl_.iter().zip(iface.iter()) {
        gen.builder
            .add_alias(g_iface, &ASTType::Ident(g_impl.clone()));
    }
    Ok(())
}

/// Ensures that the implemented interface method matches the expected signature.
fn check_equal_signature(
    gen: &mut MIRGenerator,
    method: &Function,
    mir_method: Rc<Variable>,
    iface_method: &IFaceMethod,
) -> Res<()> {
    let mir_method = if let Type::Function(method) = &mir_method.type_ {
        method
    } else {
        panic!()
    };
    let mir_method = mir_method.borrow();

    if mir_method.ret_type != gen.builder.translate_generic(&iface_method.ret_type) {
        let tok = method
            .sig
            .return_type
            .as_ref()
            .map(|t| t.get_token())
            .flatten_();
        return Err(MIRGenerator::anon_err(
            gen,
            tok,
            "Incorrect return type on interface method.",
        ));
    }

    for (i, (method_param, iface_param)) in mir_method
        .parameters
        .iter()
        .zip(iface_method.parameters.iter())
        .enumerate()
    {
        let iface_ty = gen.builder.translate_generic(iface_param);
        if method_param.type_ != iface_ty {
            let tok = &method.sig.parameters[i].name;
            return Err(MIRGenerator::error(
                gen,
                tok,
                tok,
                &format!("Incorrect parameter type on interface method (Expected {}).", iface_ty),
            ));
        }
    }

    Ok(())
}
