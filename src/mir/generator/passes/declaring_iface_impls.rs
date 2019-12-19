/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/19/19 7:35 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::ast;
use crate::ast::Module;
use crate::error::{Error, Errors, Res};
use crate::mir::generator::builder::MIRBuilder;
use crate::mir::generator::passes::PreMIRPass;
use crate::mir::nodes::{IFaceImpl, IFaceImpls, Type};
use crate::mir::{get_iface_impls, mutrc_new, MModule, MutRc, IFACE_IMPLS};

/// This pass inserts all iface impls in the global impl
/// table. It only validates that the type implementing for
/// exists, no other checks are performed.
pub struct DeclareIfaceImpls();

impl PreMIRPass for DeclareIfaceImpls {
    fn run(
        &mut self,
        ast: &mut Module,
        module: MutRc<MModule>,
        _modules: &[MutRc<MModule>],
    ) -> Result<(), Errors> {
        let mut errs = Vec::new();
        let mut builder = MIRBuilder::new(&module);

        for im in ast.iface_impls.drain(..) {
            declare_impl(im, &mut builder, None)
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

fn declare_impl(
    iface_impl: ast::IFaceImpl,
    builder: &mut MIRBuilder,
    override_implementor: Option<Type>,
) -> Res<()> {
    let implementor = override_implementor
        .map(|i| Ok(i))
        .unwrap_or_else(|| builder.find_type(&iface_impl.implementor))?;
    let ty = builder.find_type(&iface_impl.iface)?;
    let iface = if let Type::Interface(iface) = ty.clone() {
        iface
    } else {
        return Err(Error::new(
            &iface_impl.iface.get_token(),
            "MIR",
            "Not an interface".to_string(),
            &builder.path,
        ));
    };

    let impls = get_or_create_iface_impls(&implementor);
    let mir_impl = IFaceImpl {
        implementor,
        iface,
        methods: IndexMap::with_capacity(iface_impl.methods.len()),
        module: Rc::clone(&builder.module),
        ast: Rc::new(iface_impl),
    };
    impls.borrow_mut().interfaces.insert(ty, mir_impl);

    Ok(())
}

/// Gets the interfaces implemented by a type.
pub fn get_or_create_iface_impls(ty: &Type) -> MutRc<IFaceImpls> {
    match get_iface_impls(ty) {
        Some(impls) => impls,
        None => IFACE_IMPLS.with(|impls| {
            let iface_impls = mutrc_new(IFaceImpls {
                implementor: ty.clone(),
                interfaces: HashMap::with_capacity(2),
                methods: HashMap::with_capacity(2),
            });
            impls
                .borrow_mut()
                .insert(ty.clone(), Rc::clone(&iface_impls));
            iface_impls
        }),
    }
}
