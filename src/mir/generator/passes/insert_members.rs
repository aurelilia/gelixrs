/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 1/26/20 10:42 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::{
    error::Res,
    lexer::token::TType,
    mir::{
        generator::{
            intrinsics::INTRINSICS,
            passes::{ModulePass, PassType},
            MIRGenerator,
        },
        get_iface_impls,
        nodes::{ADTMember, ADTType, CastType, Expr, Type, ADT},
        MutRc,
    },
};
use std::collections::HashMap;

/// This pass fills all ADTs with their members
/// and creates their internal init function.
pub struct InsertClassMembers();

impl ModulePass for InsertClassMembers {
    fn get_type(&self) -> PassType {
        PassType::Type
    }

    fn run_type(&self, gen: &mut MIRGenerator, ty: Type) -> Res<()> {
        if let Type::Adt(adt) = ty {
            fill_adt(gen, adt)?
        }
        Ok(())
    }
}

fn fill_adt(gen: &mut MIRGenerator, adt: MutRc<ADT>) -> Res<()> {
    if adt.borrow().ty.needs_lifecycle() {
        build_adt(gen, &adt)?;
        build_destructor(gen, &adt);
        build_sr_destructor(gen, &adt);
        check_duplicate(gen, &adt)?;
    }
    Ok(())
}

/// This function will fill the ADT with its members while also generating the init method.
fn build_adt(gen: &mut MIRGenerator, adt: &MutRc<ADT>) -> Res<()> {
    let ast = Rc::clone(&adt.borrow().ast);
    let adt_variable = {
        let inst = adt
            .borrow()
            .instantiator
            .as_ref()
            .unwrap()
            .type_
            .as_function()
            .clone();
        gen.set_pointer(inst.clone());
        let inst = inst.borrow();
        Rc::clone(&inst.parameters[0])
    };

    let offset = adt.borrow().members.len();
    for (i, field) in ast.members().unwrap().iter().enumerate() {
        let value = field.initializer.as_ref().map(|e| gen.expression(e));
        let value = match value {
            Some(v) => Some(v?),
            None => None,
        };
        let type_ = value.as_ref().map_or_else(
            || gen.builder.find_type(field.ty.as_ref().unwrap()),
            |v| Ok(v.get_type()),
        )?;

        if !type_.can_escape() {
            return Err(gen.err(&field.name, "ADT field may not be a weak reference"));
        }

        let member = Rc::new(ADTMember {
            mutable: field.mutable,
            visible: true,
            type_,
            index: i + offset,
            has_default_value: field.initializer.is_some(),
        });

        let existing_entry = adt
            .borrow_mut()
            .members
            .insert(Rc::clone(&field.name.lexeme), Rc::clone(&member));
        if existing_entry.is_some() {
            return Err(gen.err(&field.name, "Class member cannot be defined twice"));
        }

        if let Some(value) = value {
            gen.insert_at_ptr(Expr::StructSet {
                object: Box::new(Expr::load(&adt_variable)),
                index: member.index,
                value: Box::new(value),
                first_set: true,
            });
        }
    }

    // Insert at the end of the instanciator to prevent
    // an edge case of an empty instanciator, which IR would interpret
    // incorrectly as an external function
    gen.insert_at_ptr(Expr::none_const());

    Ok(())
}

fn build_destructor(gen: &mut MIRGenerator, adt: &MutRc<ADT>) {
    let adt_variable = {
        let dest = adt
            .borrow()
            .destructor
            .as_ref()
            .unwrap()
            .type_
            .as_function()
            .clone();
        let func = dest.borrow();
        gen.set_pointer(dest.clone());
        Rc::clone(&func.parameters[0])
    };

    // Hacky workaround to fix LLVM IR issue:
    // if a type does not have any fields, the WR destructor is kept empty in MIR,
    // causing LLVM to try and link it (and fail) as it is never generated in IR.
    // Putting this makes the compiler (correctly) produce an empty function.
    gen.insert_at_ptr(Expr::none_const());

    let free_iface = INTRINSICS.with(|i| i.borrow().free_iface.clone()).unwrap();
    let free_method = get_iface_impls(&Type::Adt(Rc::clone(adt)))
        .map(|impls| {
            impls
                .borrow()
                .interfaces
                .get(&free_iface)
                .map(|iface| Rc::clone(iface.methods.get_index(0).unwrap().1))
        })
        .flatten()
        .or_else(|| {
            get_iface_impls(&Type::Weak(Rc::clone(adt)))
                .map(|impls| {
                    impls
                        .borrow()
                        .interfaces
                        .get(&free_iface)
                        .map(|iface| Rc::clone(iface.methods.get_index(0).unwrap().1))
                })
                .flatten()
        });
    if let Some(method) = free_method {
        gen.insert_at_ptr(Expr::call(
            Expr::load(&method),
            vec![Expr::load(&adt_variable)],
        ));
    }

    let adt = adt.borrow();
    if let ADTType::Enum { cases } = &adt.ty {
        gen.insert_at_ptr(build_enum_destructor(Expr::load(&adt_variable), cases))
    } else {
        for field in adt.members.values() {
            if let Type::Value(adt) | Type::Adt(adt) = &field.type_ {
                gen.insert_at_ptr(Expr::mod_rc(
                    Expr::cast(
                        Expr::struct_get(Expr::load(&adt_variable), field),
                        &Type::Weak(Rc::clone(adt)),
                        if field.type_.is_adt() {
                            CastType::SRtoWR
                        } else {
                            CastType::DVtoWR
                        },
                    ),
                    true,
                ));
            }
        }
    }
}

fn build_sr_destructor(gen: &mut MIRGenerator, adt_rc: &MutRc<ADT>) {
    let (adt_variable, dealloc_var) = {
        let dest = adt_rc
            .borrow()
            .destructor_sr
            .as_ref()
            .unwrap()
            .type_
            .as_function()
            .clone();
        let func = dest.borrow();
        gen.set_pointer(dest.clone());
        (
            Rc::clone(&func.parameters[0]),
            Rc::clone(&func.parameters[1]),
        )
    };

    let mut if_free_exprs = Vec::with_capacity(adt_rc.borrow().members.len() + 3);
    if_free_exprs.push(Expr::mod_rc(Expr::load(&adt_variable), false));

    let adt = adt_rc.borrow();
    if_free_exprs.push(Expr::call(
        Expr::load(adt.destructor.as_ref().unwrap()),
        vec![Expr::cast(
            Expr::load(&adt_variable),
            &Type::Weak(Rc::clone(adt_rc)),
            CastType::SRtoWR,
        )],
    ));
    if_free_exprs.push(Expr::Free(Box::new(Expr::load(&adt_variable))));

    gen.insert_at_ptr(Expr::if_(
        Expr::load(&dealloc_var),
        Expr::Block(if_free_exprs),
        Expr::none_const(),
        false,
    ));
}

/// Builds an enum destructor. Instead of just decrementing all
/// members, an enum destructor must instead switch on itself
/// to figure out which case destructor to call.
fn build_enum_destructor(enu: Expr, cases: &HashMap<Rc<String>, MutRc<ADT>>) -> Expr {
    let mut when_brs = Vec::with_capacity(cases.len());
    for case in cases.values() {
        let case_ty = Type::Adt(Rc::clone(&case));
        // `then` and `cond` are in reverse order to prevent a "use after move" borrowck error
        let then = Expr::call(
            Expr::load(case.borrow().destructor.as_ref().unwrap()),
            vec![Expr::cast(enu.clone(), &case_ty, CastType::Bitcast)],
        );
        let cond = Expr::binary(enu.clone(), TType::Is, Expr::type_get(case_ty));
        when_brs.push((cond, then))
    }
    Expr::when(when_brs, None, Type::None)
}

fn check_duplicate(gen: &mut MIRGenerator, adt: &MutRc<ADT>) -> Res<()> {
    let adt = adt.borrow();
    for (mem_name, _) in adt.members.iter() {
        if adt.methods.contains_key(mem_name) {
            return Err(gen.err(
                &adt.ast.name,
                &format!(
                    "Cannot have member and method '{}' with same name.",
                    mem_name
                ),
            ));
        }
    }
    Ok(())
}
