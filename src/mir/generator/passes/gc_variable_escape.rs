/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/28/19 10:28 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::{
    error::Res,
    mir::{
        generator::{
            passes::{ModulePass, PassType},
            MIRGenerator,
        },
        nodes::{Expr, Flow, Function, Type},
        MutRc,
    },
};

/// This pass generates the bodies of all functions and methods.
pub struct GCMarkEscapeVariables();

impl ModulePass for GCMarkEscapeVariables {
    fn get_type(&self) -> PassType {
        PassType::Type
    }

    fn run_type(&self, _gen: &mut MIRGenerator, ty: Type) -> Res<()> {
        match ty {
            Type::Function(func) => mark_escaping_vars(&func),

            Type::Class(_class) => (),

            Type::Interface(_iface) => (),

            _ => panic!("Primitive type in module!"),
        }
        Ok(())
    }
}

fn mark_escaping_vars(function: &MutRc<Function>) {
    function.borrow_mut().gc_inspected = true;

    for block in function.borrow().blocks.values() {
        for inst in block.iter() {
            mark_escaping_vars_expr(inst, false);
        }
    }

    // Run a second time to backpropagate variables
    // that were only marked on-heap later
    for block in function.borrow().blocks.values() {
        for inst in block.iter() {
            mark_escaping_vars_expr(inst, false);
        }
    }
}

fn mark_escaping_vars_expr(expr: &Expr, mut escaped: bool) -> bool {
    match expr {
        Expr::Allocate { heap, .. } => {
            // TODO: Should an allocation really pass on its allocation
            // status in `escaped`? Or is this not needed?
            escaped = heap.get() || escaped;
            heap.set(escaped);
        }

        Expr::Binary { left, right, .. } => {
            mark_escaping_vars_expr(left, escaped);
            mark_escaping_vars_expr(right, escaped);
        }

        Expr::Call { callee, arguments } => {
            mark_escaping_vars_expr(callee, false);

            let function = callee.get_type();
            let function = match function {
                Type::Function(func) => func,
                Type::Closure(_clo) => {
                    for arg in arguments {
                        mark_escaping_vars_expr(arg, true);
                    }
                    return escaped;
                }
                _ => panic!("Not a valid callee"),
            };

            if !function.borrow().gc_inspected {
                mark_escaping_vars(&function);
            }

            if function.borrow().blocks.is_empty() {
                escaped = true;
            };

            for (param, arg) in function.borrow().parameters.iter().zip(arguments.iter()) {
                mark_escaping_vars_expr(arg, param.escapes.get());
            }
        }

        Expr::CallDyn { arguments, .. } => {
            // TODO: Might need to be `true` instead of `escaped`,
            // not knowable if a value will escape due to dynamic dispatch
            for arg in arguments.iter() {
                mark_escaping_vars_expr(arg, escaped);
            }
        }

        Expr::CastToInterface {
            object,
            to: _,
            store,
        } => {
            mark_escaping_vars_expr(object, escaped);
            mark_escaping_vars_expr(store, escaped);
        }

        Expr::ConstructClosure { captured, .. } => {
            for var in captured.iter() {
                var.escapes.set(true)
            }
        }

        Expr::Flow(flow) => {
            match &**flow {
                Flow::Return(value) => mark_escaping_vars_expr(value, true),
                _ => escaped,
            };
        }

        Expr::ModifyRefCount { object, .. } => {
            escaped = mark_escaping_vars_expr(object, escaped);
        }

        Expr::Phi(branches) => {
            for (expr, _) in branches.iter() {
                mark_escaping_vars_expr(expr, escaped);
            }
        }

        Expr::StructGet { object, .. } => {
            mark_escaping_vars_expr(object, escaped);
        }

        Expr::StructSet { object, value, .. } => {
            escaped = mark_escaping_vars_expr(object, escaped);
            mark_escaping_vars_expr(value, escaped);
        }

        Expr::Literal(_) => (),

        Expr::Unary { right, .. } => {
            mark_escaping_vars_expr(right, escaped);
        }

        Expr::VarGet(var) => {
            escaped = escaped || var.escapes.get();
            var.escapes.set(escaped)
        }

        Expr::VarStore { var, value } => {
            escaped = escaped || var.escapes.get();
            var.escapes.set(escaped);
            mark_escaping_vars_expr(value, escaped);
        }
    }
    escaped
}
