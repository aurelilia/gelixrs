/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/16/19 2:53 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::ast::Type as ASTType;
use crate::error::Res;
use crate::mir::generator::passes::{ModulePass, PassType};
use crate::mir::generator::MIRGenerator;
use crate::mir::nodes::{Class, ClassMember, Expr, Type, Variable};
use crate::mir::{MModule, MutRc};

/// This pass fills all classes with their members
/// and creates their internal init function.
pub struct InsertClassMembers();

impl ModulePass for InsertClassMembers {
    fn get_type(&self) -> PassType {
        PassType::Type
    }

    fn run_type(&mut self, module: &MutRc<MModule>, ty: Type) -> Res<()> {
        if let Type::Class(cls) = ty {
            let mut gen = MIRGenerator::new(module);
            fill_class(&mut gen, cls)?
        }
        Ok(())
    }
}

fn fill_class(gen: &mut MIRGenerator, class: MutRc<Class>) -> Res<()> {
    build_class(gen, &class)?;
    check_duplicate(gen, &class)
}

/// This function will fill the class with its members while also generating the init method.
fn build_class(gen: &mut MIRGenerator, class: &MutRc<Class>) -> Res<()> {
    let ast = Rc::clone(&class.borrow().ast);
    {
        let inst = class.borrow().instantiator.type_.as_function().clone();
        let mut func = inst.borrow_mut();
        gen.set_pointer(inst.clone(), func.append_block("entry", false));
    }

    let class_variable = Rc::new(Variable {
        mutable: true,
        type_: gen
            .builder
            .find_type(&ASTType::Ident(ast.name.clone()))
            .ok()
            .unwrap(),
        name: Rc::new("this".to_string()),
    });
    gen.add_function_variable(Rc::clone(&class_variable));

    let offset = class.borrow().members.len();
    for (i, field) in ast.variables.iter().enumerate() {
        let value = field.initializer.as_ref().map(|e| gen.expression(e));
        let value = match value {
            Some(v) => Some(v?),
            None => None,
        };
        let type_ = value
            .as_ref()
            .map(|v| Ok(v.get_type()))
            .unwrap_or_else(|| gen.builder.find_type(field.ty.as_ref().unwrap()))?;

        let member = Rc::new(ClassMember {
            mutable: field.mutable,
            type_,
            index: (i + offset) as u32,
            has_default_value: field.initializer.is_some(),
        });

        let existing_entry = class
            .borrow_mut()
            .members
            .insert(Rc::clone(&field.name.lexeme), Rc::clone(&member));
        if existing_entry.is_some() {
            return Err(gen.err(&field.name, "Class member cannot be defined twice"));
        }

        if let Some(value) = value {
            gen.insert_at_ptr(Expr::struct_set(Expr::load(&class_variable), member, value));
        }
    }

    gen.insert_at_ptr(Expr::ret(Expr::load(&class_variable)));
    Ok(())
}

fn check_duplicate(gen: &mut MIRGenerator, class: &MutRc<Class>) -> Res<()> {
    let class = class.borrow();
    for (mem_name, _) in class.members.iter() {
        if class.methods.contains_key(mem_name) {
            return Err(gen.err(
                &class.ast.name,
                &format!(
                    "Cannot have class member and method '{}' with same name.",
                    mem_name
                ),
            ));
        }
    }
    Ok(())
}
