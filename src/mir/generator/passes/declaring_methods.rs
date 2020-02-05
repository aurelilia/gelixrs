/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 2/3/20 7:27 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::{collections::HashSet, rc::Rc};

use either::Either::Left;

use crate::{
    ast::{
        declaration::{Constructor, FuncSignature, FunctionParam, Visibility},
        Class as ASTClass, Type as ASTType,
    },
    error::{Error, Res},
    lexer::token::Token,
    mir::{
        generator::{
            builder::MIRBuilder,
            passes::{declaring_globals::create_function, ModulePass, PassType},
            MIRGenerator,
        },
        nodes::{Class, Expr, IFaceMethod, Interface, Type, Variable},
        result::ToMIRResult,
        MutRc,
    },
};

/// This pass defines all methods on classes and interfaces.
pub struct DeclareMethods();

impl ModulePass for DeclareMethods {
    fn get_type(&self) -> PassType {
        PassType::Type
    }

    fn run_type(&self, gen: &mut MIRGenerator, ty: Type) -> Result<(), Error> {
        match ty {
            Type::Class(cls) => declare_for_class(&mut gen.builder, cls)?,
            Type::Interface(iface) => declare_for_iface(&mut gen.builder, iface)?,
            _ => (),
        }
        Ok(())
    }
}

fn declare_for_class(builder: &mut MIRBuilder, class: MutRc<Class>) -> Res<()> {
    let ast = Rc::clone(&class.borrow().ast);
    let this_param = FunctionParam::this_param(&ast.name);

    // Do the instantiator & destructor
    let init_fn_sig = get_instantiator_fn_sig(&ast, this_param.clone());
    class.borrow_mut().instantiator = create_function(builder, Left(&init_fn_sig), false, None)?;
    let free_fn_sig = get_destructor_fn_sig(&ast, this_param.clone());
    class.borrow_mut().destructor = create_function(builder, Left(&free_fn_sig), false, None)?;

    // Do all user-defined methods
    for method in ast.methods.iter() {
        let mir_method =
            create_function(builder, Left(&method.sig), false, Some(this_param.clone()))?;
        class
            .borrow_mut()
            .methods
            .insert(Rc::clone(&method.sig.name.lexeme), mir_method);
    }

    // Do all constructors
    let mut constructor_list = HashSet::with_capacity(ast.constructors.len());

    let default = maybe_default_constructor(&ast);
    let iter = ast.constructors.iter().chain(default.iter()).enumerate();
    for (i, constructor) in iter {
        let sig = get_constructor_sig(builder, &ast, constructor, &this_param, i)?;
        let mir_var = create_function(builder, Left(&sig), false, None)?;
        let mir_fn = mir_var.type_.as_function();
        let mut mir_fn = mir_fn.borrow_mut();
        mir_fn.exprs = insert_constructor_setters(builder, &ast, constructor, &mir_fn.parameters)?;
        class.borrow_mut().constructors.push(Rc::clone(&mir_var));

        let params = mir_fn
            .parameters
            .iter()
            .skip(1)
            .map(|p| &p.type_)
            .cloned()
            .collect::<Vec<Type>>();
        if !constructor_list.insert(params) {
            return Err(Error::new(
                &ast.name,
                "MIR",
                "Class contains constructors with duplicate signatures.".to_string(),
                &builder.path,
            ));
        }
    }

    Ok(())
}

/// Returns signature of the class instantiator.
fn get_instantiator_fn_sig(class: &ASTClass, this_param: FunctionParam) -> FuncSignature {
    let fn_name = Token::generic_identifier(format!("create-{}-instance", &class.name.lexeme));
    FuncSignature {
        name: fn_name,
        visibility: Visibility::Public,
        generics: None,
        return_type: None,
        parameters: vec![this_param],
        variadic: false,
    }
}

/// Returns signature of the class instantiator.
fn get_destructor_fn_sig(class: &ASTClass, this_param: FunctionParam) -> FuncSignature {
    let fn_name = Token::generic_identifier(format!("free-{}-instance", &class.name.lexeme));
    FuncSignature {
        name: fn_name,
        visibility: Visibility::Public,
        generics: None,
        return_type: None,
        parameters: vec![
            this_param,
            FunctionParam {
                type_: ASTType::Ident(Token::generic_identifier("bool".to_string())),
                name: Token::generic_identifier("refcount_is_0".to_string()),
            },
        ],
        variadic: false,
    }
}

/// Returns the MIR function signature of a class constructor.
fn get_constructor_sig(
    builder: &mut MIRBuilder,
    class: &ASTClass,
    constructor: &Constructor,
    this_arg: &FunctionParam,
    index: usize,
) -> Res<FuncSignature> {
    let name = Token::generic_identifier(format!("{}-constructor-{}", &class.name.lexeme, index));
    let mut parameters = constructor
        .parameters
        .iter()
        .map(|(name, ty)| {
            let type_ = ty
                .clone()
                .or_else(|| {
                    get_field_by_name(class, name)
                        .map(|(_, ty)| ty)
                        .and_then(|t| t)
                })
                .or_err(
                    &builder.path,
                    name,
                    "Cannot infer type of field with default value (specify type explicitly.)",
                )?;
            Ok(FunctionParam {
                type_,
                name: name.clone(),
            })
        })
        .collect::<Res<Vec<FunctionParam>>>()?;
    parameters.insert(0, this_arg.clone());
    Ok(FuncSignature {
        name,
        visibility: constructor.visibility,
        generics: None,
        return_type: None,
        parameters,
        variadic: false,
    })
}

fn get_field_by_name(class: &ASTClass, name: &Token) -> Option<(usize, Option<ASTType>)> {
    class
        .variables
        .iter()
        .enumerate()
        .find(|(_, mem)| mem.name.lexeme == name.lexeme)
        .map(|(i, mem)| (i, mem.ty.clone()))
}

/// Insert all constructor 'setter' parameters into the entry
/// block of the MIR function.
fn insert_constructor_setters(
    builder: &mut MIRBuilder,
    class: &ASTClass,
    constructor: &Constructor,
    mir_fn_params: &[Rc<Variable>],
) -> Res<Vec<Expr>> {
    let mut block = Vec::new();
    for (index, (param, _)) in constructor
        .parameters
        .iter()
        .enumerate()
        .filter(|(_, (_, ty))| ty.is_none())
    {
        let (field_index, _) =
            get_field_by_name(class, param).or_err(&builder.path, param, "Unknown class field.")?;
        block.push(Expr::struct_set(
            Expr::load(&mir_fn_params[0]),
            field_index,
            Expr::load(&mir_fn_params[index + 1]),
            true,
        ))
    }
    block.push(Expr::none_const());
    Ok(block)
}

/// Will return a default constructor with no parameters
/// should the class not contain a constructor and
/// all members have default values.
fn maybe_default_constructor(class: &ASTClass) -> Option<Constructor> {
    let no_uninitialized_members = !class.variables.iter().any(|v| v.initializer.is_none());
    if class.constructors.is_empty() && no_uninitialized_members {
        Some(Constructor {
            parameters: vec![],
            visibility: Visibility::Public,
            body: None,
        })
    } else {
        None
    }
}

fn declare_for_iface(builder: &mut MIRBuilder, iface: MutRc<Interface>) -> Res<()> {
    let ast = Rc::clone(&iface.borrow().ast);
    for method in ast.methods.iter() {
        let ret_type = match method.sig.return_type.as_ref() {
            Some(ty) => builder.find_type(ty)?,
            None => Type::None,
        };

        let mut parameters = Vec::with_capacity(method.sig.parameters.len());
        for param in method.sig.parameters.iter() {
            parameters.push(builder.find_type(&param.type_)?);
        }

        let dupe = iface.borrow_mut().methods.insert(
            Rc::clone(&method.sig.name.lexeme),
            IFaceMethod {
                name: Rc::clone(&method.sig.name.lexeme),
                parameters,
                ret_type,
                has_default_impl: method.body.is_some(),
            },
        );

        if dupe.is_some() {
            return Err(Error::new(
                &method.sig.name,
                "MIR",
                format!(
                    "Method with name {} already defined",
                    method.sig.name.lexeme
                ),
                &builder.path,
            ));
        }
    }
    Ok(())
}
