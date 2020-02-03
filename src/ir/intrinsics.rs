/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 2/3/20 1:58 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::{
    ir::{IRGenerator, PtrEqRc},
    mir::{nodes::Function, MModule, MutRc},
};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::FunctionValue,
    AddressSpace::Generic,
    IntPredicate,
};
use std::cell::RefMut;

impl IRGenerator {
    pub(super) fn fill_intrinsic_functions(&mut self, module: &MutRc<MModule>) {
        for global in module.borrow().globals.values() {
            let ir = self.functions[&PtrEqRc::new(global)];
            let func = global.type_.as_function();
            self.fill_intrinsic(func.borrow_mut(), ir)
        }
    }

    fn fill_intrinsic(&mut self, func: RefMut<Function>, ir: FunctionValue) {
        // Trim the module name and generic parameters off the function
        // "std/mod:func_name<A, B>" -> "func_name"
        let name = &func
            .name
            .rsplit(':')
            .next()
            .unwrap()
            .split('<')
            .next()
            .unwrap();
        // All functions intended to be generated in IR have this prefix
        if name.len() < 8 || &name[..8] != "gelixrs_" {
            return;
        }

        self.prepare_function(&func, ir);

        // Strip the "gelixrs_" all IR functions start with
        match &name[8..] {
            // Returns the size of the type. Implemented by
            // indexing a null pointer (0x0) at element 1
            // and returning the offset from 0.
            // Credit: http://nondot.org/sabre/LLVMNotes/SizeOf-OffsetOf-VariableSizedStructs.txt
            "get_type_size" => {
                let i = self.context.i64_type();
                let mir_ty = func.context.type_aliases.values().next().unwrap();
                let typ = self.ir_ty(&mir_ty).ptr_type(Generic);
                let ptr_size = unsafe {
                    self.builder
                        .build_gep(typ.const_null(), &[i.const_int(1, false)], "size")
                };
                self.builder
                    .build_return(Some(&self.builder.build_ptr_to_int(ptr_size, i, "sizeI")));
            }

            // Take a pointer and return the value behind it.
            // ADTs are already pointers, so they are no-op.
            "deref_ptr" => {
                let ret_ty = ir.get_type().get_return_type().unwrap();
                let int = ir.get_first_param().unwrap().into_int_value();

                let ret_val = if let BasicTypeEnum::PointerType(ret_ty) = ret_ty {
                    self.builder.build_int_to_ptr(int, ret_ty, "ptr").into()
                } else {
                    let ptr = self
                        .builder
                        .build_int_to_ptr(int, ret_ty.ptr_type(Generic), "ptr");
                    self.builder.build_load(ptr, "load-ptr")
                };
                self.builder.build_return(Some(&ret_val));
            }

            // Dereference a pointer and set the value at its location
            // to the given value.
            "set_ptr" => {
                let value = ir.get_last_param().unwrap();
                let ty = value.get_type();
                let int = ir.get_first_param().unwrap().into_int_value();

                if let BasicTypeEnum::PointerType(ty) = ty {
                    let ptr = self.builder.build_int_to_ptr(int, ty, "ptr");
                    let value = self
                        .builder
                        .build_load(value.into_pointer_value(), "load-value");
                    self.build_store(ptr, value, false); // TODO: Is false correct here?
                } else {
                    let ptr = self
                        .builder
                        .build_int_to_ptr(int, ty.ptr_type(Generic), "ptr");
                    self.build_store(ptr, value, false);
                }
                self.builder.build_return(None);
            }

            "inc_ref_iface" => {
                let vtable_ptr = self.builder.build_int_to_ptr(
                    ir.get_last_param().unwrap().into_int_value(),
                    self.context
                        .struct_type(
                            &[self
                                .context
                                .void_type()
                                .fn_type(
                                    &[
                                        self.context.i64_type().ptr_type(Generic).into(),
                                        self.context.bool_type().into(),
                                    ],
                                    false,
                                )
                                .ptr_type(Generic)
                                .into()],
                            false,
                        )
                        .ptr_type(Generic),
                    "cast",
                );

                let func = self
                    .load_ptr(self.struct_gep(vtable_ptr, 0))
                    .into_pointer_value();

                let func_as_i64 =
                    self.builder
                        .build_ptr_to_int(func, self.context.i64_type(), "free_to_int");
                let impl_is_primitive = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    func_as_i64,
                    self.context.i64_type().const_int(0, false),
                    "impl_is_primitve",
                );

                let cont_bb = ir.append_basic_block("do");
                let end_bb = ir.append_basic_block("end");
                self.builder
                    .build_conditional_branch(impl_is_primitive, &end_bb, &cont_bb);

                self.builder.position_at_end(&cont_bb);
                let impl_ptr_i64 = self.builder.build_int_to_ptr(
                    ir.get_first_param().unwrap().into_int_value(),
                    self.context.i64_type().ptr_type(Generic),
                    "impl",
                );
                let impl_ptr = self
                    .builder
                    .build_bitcast(
                        impl_ptr_i64,
                        self.context.i32_type().ptr_type(Generic),
                        "impl_to_i32ptr",
                    )
                    .into_pointer_value();

                let rc = self.builder.build_load(impl_ptr, "rcload").into_int_value();
                let added = self.context.i32_type().const_int(1, false).into();
                let new_rc = self.builder.build_int_add(rc, added, "rcinc");
                self.builder.build_store(impl_ptr, new_rc);
                self.builder.build_unconditional_branch(&end_bb);

                self.builder.position_at_end(&end_bb);
                self.builder.build_return(None);
            }

            "dec_ref_iface" => {
                let vtable_ptr = self.builder.build_int_to_ptr(
                    ir.get_last_param().unwrap().into_int_value(),
                    self.context
                        .struct_type(
                            &[self
                                .context
                                .void_type()
                                .fn_type(
                                    &[
                                        self.context.i64_type().ptr_type(Generic).into(),
                                        self.context.bool_type().into(),
                                    ],
                                    false,
                                )
                                .ptr_type(Generic)
                                .into()],
                            false,
                        )
                        .ptr_type(Generic),
                    "cast",
                );

                let func = self
                    .load_ptr(self.struct_gep(vtable_ptr, 0))
                    .into_pointer_value();

                let func_as_i64 =
                    self.builder
                        .build_ptr_to_int(func, self.context.i64_type(), "free_to_int");
                let impl_is_primitive = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    func_as_i64,
                    self.context.i64_type().const_int(0, false),
                    "impl_is_primitve",
                );

                let cont_bb = ir.append_basic_block("do");
                let end_bb = ir.append_basic_block("end");
                self.builder
                    .build_conditional_branch(impl_is_primitive, &end_bb, &cont_bb);

                self.builder.position_at_end(&cont_bb);
                let impl_ptr_i64 = self.builder.build_int_to_ptr(
                    ir.get_first_param().unwrap().into_int_value(),
                    self.context.i64_type().ptr_type(Generic),
                    "impl",
                );
                let impl_ptr = self
                    .builder
                    .build_bitcast(
                        impl_ptr_i64,
                        self.context.i32_type().ptr_type(Generic),
                        "impl_to_i32ptr",
                    )
                    .into_pointer_value();

                let rc = self.builder.build_load(impl_ptr, "rcload").into_int_value();
                let added = self.context.i32_type().const_int(1, false).into();
                let new_rc = self.builder.build_int_sub(rc, added, "rcdec");
                self.builder.build_store(impl_ptr, new_rc);
                let rc_is_0 = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    new_rc,
                    self.context.i32_type().const_int(0, false),
                    "rc_is_0",
                );
                self.builder
                    .build_call(func, &[impl_ptr_i64.into(), rc_is_0.into()], "free_call");
                self.builder.build_unconditional_branch(&end_bb);

                self.builder.position_at_end(&end_bb);
                self.builder.build_return(None);
            }

            _ => panic!("Unknown intrinsic function: {}", name),
        }
    }
}
