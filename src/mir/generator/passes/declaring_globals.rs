/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/15/19 9:58 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::cell::RefCell;
use std::rc::Rc;

use either::Either;

use crate::ast;
use crate::ast::declaration::FuncSignature;
use crate::ast::Module;
use crate::ast::module::ModulePath;
use crate::error::{Error, Errors, Res};
use crate::mir::{MModule, MutRc, mutrc_new};
use crate::mir::generator::builder::MIRBuilder;
use crate::mir::generator::intrinsics::INTRINSICS;
use crate::mir::generator::passes::{ModulePass, PassType, PreMIRPass};
use crate::mir::nodes::{Function, Type, Variable};
use crate::mir::result::ToMIRResult;

/// This pass defines all globals inside the module; currently only functions.
/// It only creates a signature and inserts it into the module;
/// no actual body/code is generated at this stage.
/// Since it requires types for its signature, this pass has to run
/// after [DeclareTypes].
pub struct DeclareGlobals();

impl PreMIRPass for DeclareGlobals {
    fn run(&mut self, ast: &mut Module, module: MutRc<MModule>) -> Result<(), Errors> {
        let mut errs = Vec::new();
        let builder = MIRBuilder::new(&module);

        for function in ast.functions.drain(..) {
            let is_ext = function.body.is_none();
            create_function(&module, &builder, Either::Right(function), is_ext)
                .map_err(|e| errs.push(e))
                .ok();
        }

        if errs.is_empty() {
            Ok(())
        } else {
            Err(Errors(errs, Rc::clone(&module.borrow().src)))
        }
    }
}

pub fn create_function(
    module: &MutRc<MModule>,
    builder: &MIRBuilder,
    func: Either<&FuncSignature, ast::Function>,
    is_external: bool,
) -> Res<Rc<Variable>> {
    let func_sig = match &func {
        Either::Left(sig) => sig,
        Either::Right(func) => &func.sig,
    };

    let name = func_sig.name.clone();
    module.borrow_mut().try_reserve_name(&name)?;

    let name_str = if is_external {
        String::clone(&name.lexeme)
    } else {
        get_function_name(&module.borrow().path, &name.lexeme)
    };

    let ret_type = func_sig
        .return_type
        .as_ref()
        .map(|ty| builder.find_type(ty))
        .unwrap_or(Ok(Type::None))?;

    let mut parameters = Vec::with_capacity(func_sig.parameters.len());
    for param in func_sig.parameters.iter() {
        parameters.push(Rc::new(Variable {
            mutable: false,
            name: Rc::clone(&param.name.lexeme),
            type_: builder.find_type(&param.type_)?,
        }));
    }

    let function = mutrc_new(Function {
        name: name_str,
        parameters,
        blocks: Default::default(),
        variables: Default::default(),
        ret_type,
        ast: func.right().map(Rc::new)
    });

    let global = Rc::new(Variable {
        name: Rc::clone(&name.lexeme),
        type_: Type::Function(Rc::clone(&function)),
        mutable: false,
    });

    if &name.lexeme[..] == "main" {
        INTRINSICS
            .with(|i| i.borrow_mut().set_main_fn(&global))
            .or_err(
                &module.borrow().path,
                &name,
                "Can't define main multiple times.",
            )?;
    }

    module
        .borrow_mut()
        .globals
        .insert(Rc::clone(&name.lexeme), Rc::clone(&global));
    Ok(global)
}

pub fn get_function_name(path: &Rc<ModulePath>, func_name: &Rc<String>) -> String {
    if func_name.as_ref() != "main" {
        format!("{}:{}", path, func_name)
    } else {
        func_name.to_string()
    }
}
