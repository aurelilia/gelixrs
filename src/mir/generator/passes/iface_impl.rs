/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/13/19 10:15 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::ast::module::Module;
use crate::mir::generator::{MIRGenerator, Res};

/// This pass checks and defines all interface impl blocks.
pub fn iface_impl_pass(_gen: &mut MIRGenerator, list: &mut Module) -> Res<()> {
    for _iface in list.iface_impls.iter_mut() {
        //    iface_impl(gen, iface)?;
    }
    Ok(())
}
/*
fn iface_impl(gen: &mut MIRGenerator, iface_impl: &mut IFaceImpl) -> Res<()> {
    if let ASTType::Generic { token, .. } = &iface_impl.implementor {
        return Err(gen.error(
            &token,
            &token,
            "Generic arguments on implementor not supported yet.",
        ));
    }

    let implementor = gen.find_type(&iface_impl.implementor)?;
    let iface_impls = get_or_create_iface_impls(&implementor);

    let iface = gen
        .builder
        .find_iface_or_proto(&iface_impl.iface.lexeme)
        .or_err(gen, &iface_impl.iface, "Unknown interface.")?
        .map_left(Ok)
        .left_or_else(|proto| {
            iface_from_proto(gen, proto, &iface_impl.iface_generics, &iface_impl.iface)
        })?;

    let mut mir_impl = MIRImpl {
        implementor,
        iface: Rc::clone(&iface),
        methods: IndexMap::with_capacity(iface.borrow().methods.len()),
    };

    for method in iface_impl.methods.iter_mut() {
        let iface = iface.borrow();
        let iface_method = iface.methods.get(&method.sig.name.lexeme).or_err(
            gen,
            &method.sig.name,
            "Method is not defined in interface.",
        )?;
        let this_arg = FunctionArg::this_arg_(&iface_impl.implementor);

        let old_name = Rc::clone(&method.sig.name.lexeme);
        method.sig.name.lexeme = Rc::new(format!(
            "{}-{}",
            iface_impl.implementor, method.sig.name.lexeme
        ));
        method.sig.parameters.insert(0, this_arg);

        let mir_method = create_function(gen, &method.sig, false, None)?
            .left()
            .unwrap();
        mir_impl
            .methods
            .insert(Rc::clone(&old_name), Rc::clone(&mir_method));
        if iface_impls.borrow().methods.contains_key(&old_name) {
            iface_impls.borrow_mut().methods.remove(&old_name);
        } else {
            iface_impls
                .borrow_mut()
                .methods
                .insert(old_name, Rc::clone(&mir_method));
        }

        check_equal_signature(gen, method, mir_method, iface_method)?;
    }

    if iface.borrow().methods.len() > iface_impl.methods.len() {
        Err(gen.error(
            &iface_impl.iface,
            &iface_impl.iface,
            "Missing methods in interface impl.",
        ))
    } else {
        if iface_impls.borrow_mut().interfaces.insert(mir_impl) {
            Ok(())
        } else {
            Err(gen.error(&iface_impl.iface, &iface_impl.iface, "Interface already implemented for type."))
        }
    }
}

fn iface_from_proto(
    gen: &mut MIRGenerator,
    proto: MutRc<InterfacePrototype>,
    args: &Option<Vec<Token>>,
    error_tok: &Token,
) -> Res<MutRc<Interface>> {
    let args = args.as_ref().or_err(
        gen,
        error_tok,
        "Missing generic arguments on interface prototype.",
    )?;
    let args = args
        .iter()
        .map(|tok| gen.find_type(&ASTType::Ident(tok.clone())))
        .collect::<Res<Vec<Type>>>()?;

    let iface = proto.borrow_mut().build(&args);
    let iface = iface.map_err(|msg| gen.error(error_tok, error_tok, &msg))?;
    iface.borrow_mut().proto = Some(proto);
    Ok(iface)
}

/// Gets the interfaces implemented by a type.
fn get_or_create_iface_impls(ty: &Type) -> MutRc<IFaceImpls> {
    match IFACE_IMPLS.with(|impls| impls.borrow().get(ty).cloned()) {
        Some(impls) => impls,
        None => {
            IFACE_IMPLS.with(|impls| {
                let iface_impls = mutrc_new(IFaceImpls {
                    implementor: ty.clone(),
                    interfaces: HashSet::with_capacity(2),
                    methods: HashMap::with_capacity(2),
                });
                impls.borrow_mut().insert(
                    ty.clone(),
                    Rc::clone(&iface_impls),
                );
                iface_impls
            })
        }
    }
}

/// Ensures that the implemented interface method matches the expected signature.
fn check_equal_signature(
    gen: &mut MIRGenerator,
    method: &Function,
    mir_method: Rc<Variable>,
    iface_method: &IFaceMethod,
) -> Res<()> {
    let mir_method = &mir_method.type_.as_function();
    let mir_method = mir_method.borrow();

    if mir_method.ret_type != iface_method.ret_type {
        let tok = method
            .sig
            .return_type
            .as_ref()
            .map(|t| t.get_token())
            .flatten_();
        return Err(gen.anon_err(tok, "Incorrect return type on interface method."));
    }

    for (i, (method_param, iface_param)) in mir_method
        .parameters
        .iter()
        .zip(iface_method.parameters.iter())
        .enumerate()
    {
        if &method_param.type_ != iface_param {
            let tok = &method.sig.parameters[i].name;
            return Err(gen.error(
                tok,
                tok,
                &format!(
                    "Incorrect parameter type on interface method (Expected {}).",
                    iface_param
                ),
            ));
        }
    }

    Ok(())
}
*/
