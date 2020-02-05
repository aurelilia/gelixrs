/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 2/3/20 3:28 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use crate::{
    ir::{IRGenerator, PtrEqRc},
    mir::nodes::Variable,
};
use inkwell::{
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, PointerValue},
    AddressSpace::Generic,
    basic_block::BasicBlock
};
use std::rc::Rc;

impl IRGenerator {
    /// Force any type to be turned into a void pointer.
    pub fn coerce_to_void_ptr(&self, ty: BasicValueEnum) -> BasicValueEnum {
        let target = self.void_ptr();
        match ty {
            BasicValueEnum::PointerValue(ptr) => self.builder.build_bitcast(ptr, target, "bc"),

            BasicValueEnum::IntValue(int) => {
                let num = self
                    .builder
                    .build_int_z_extend(int, self.context.i64_type(), "extend");
                self.builder
                    .build_int_to_ptr(num, target, "inttoptr")
                    .into()
            }

            BasicValueEnum::FloatValue(flt) => self.coerce_to_void_ptr(self.builder.build_bitcast(
                flt,
                self.context.i64_type(),
                "flttoint",
            )),

            _ => panic!("Cannot coerce to void ptr: {:?}", ty),
        }
    }

    /// Returns the IR pointer of the variable.
    /// Can also be used for globals, although variables are
    /// searched for first.
    pub fn get_variable(&self, var: &Rc<Variable>) -> PointerValue {
        let wrap = PtrEqRc::new(var);
        self.variables
            .get(&wrap)
            .cloned()
            .unwrap_or_else(|| self.functions[&wrap].as_global_value().as_pointer_value())
    }

    /// Write a set of values to a given struct.
    pub fn write_struct<'a, T: Iterator<Item = &'a BasicValueEnum>>(
        &self,
        location: PointerValue,
        values: T,
    ) {
        for (i, value) in values.enumerate() {
            let slot = self.struct_gep(location, i);
            self.build_store(slot, *value, true);
        }
    }

    /// Loads a pointer, turning it into a value.
    /// Does not load structs or functions, since they are only ever used as pointers.
    pub fn load_ptr(&self, ptr: PointerValue) -> BasicValueEnum {
        match ptr.get_type().get_element_type() {
            AnyTypeEnum::FunctionType(_) => BasicValueEnum::PointerValue(ptr),
            AnyTypeEnum::StructType(str)
                if str.get_field_type_at_index(0) == Some(self.context.i32_type().into()) =>
            {
                BasicValueEnum::PointerValue(ptr)
            }
            _ => self.builder.build_load(ptr, "var"),
        }
    }

    /// Perform a struct GEP with some additional safety checks.
    /// The index will be offset by one should the struct contain a refcount field,
    /// so callers do not need to account for this.
    pub fn struct_gep(&self, ptr: PointerValue, index: usize) -> PointerValue {
        assert!(ptr.get_type().get_element_type().is_struct_type());

        // Account for the reference count field, should it be present
        let index = if ptr
            .get_type()
            .get_element_type()
            .as_struct_type()
            .get_field_type_at_index(0)
            == Some(self.context.i32_type().into())
        {
            index as u32 + 1
        } else {
            index as u32
        };

        assert!(
            ptr.get_type()
                .get_element_type()
                .as_struct_type()
                .count_fields()
                > index
        );

        unsafe { self.builder.build_struct_gep(ptr, index, "gep") }
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    /// The alloca is kept empty.
    pub fn create_alloc(&self, ty: BasicTypeEnum, heap: bool) -> PointerValue {
        let builder = self.context.create_builder();

        let (builder, ptr) = if heap {
            let malloc = self
                .module
                .get_function("malloc")
                .unwrap()
                .as_global_value()
                .as_pointer_value();
            let malloc_ty = ty
                .ptr_type(Generic)
                .fn_type(&[self.context.i32_type().into()], false);
            let malloc = self
                .builder
                .build_bitcast(malloc, malloc_ty.ptr_type(Generic), "malloccast")
                .into_pointer_value();

            let i = self.context.i32_type();
            let ty_size = unsafe {
                self.builder.build_gep(
                    ty.ptr_type(Generic).const_null(),
                    &[i.const_int(1, false)],
                    "size",
                )
            };
            let ty_size = self.builder.build_ptr_to_int(ty_size, i, "sizeint").into();

            (
                &self.builder,
                self.builder
                    .build_call(malloc, &[ty_size], "malloc")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value(),
            )
        } else {
            let entry = self
                .builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap()
                .get_first_basic_block()
                .unwrap();

            match entry.get_first_instruction() {
                Some(first_instr) => builder.position_before(&first_instr),
                None => builder.position_at_end(&entry),
            }

            (&builder, builder.build_alloca(ty, "tmpalloc"))
        };

        if ty.is_struct_type()
            && ty.as_struct_type().get_field_type_at_index(0)
                == Some(self.context.i32_type().into())
        {
            // Initialize the refcount to 0
            let rc = unsafe { builder.build_struct_gep(ptr, 0, "rcinit") };
            let value = if heap {
                0
            } else {
                2_147_483_648 /* first bit 1, rest 0; used to differentiate heap/stack vars */
            };
            builder.build_store(rc, self.context.i32_type().const_int(value, false));
        }
        ptr
    }

    pub fn locals(&mut self) -> &mut Vec<BasicValueEnum> {
        self.locals.last_mut().unwrap()
    }

    pub fn push_locals(&mut self) {
        self.locals.push(Vec::with_capacity(5))
    }

    pub fn pop_dec_locals(&mut self) {
        let locals = self.locals.pop().unwrap();
        self.decrement_locals(&locals)
    }

    pub fn pop_locals_lift(&mut self, lift: BasicValueEnum) {
        let mut locals = self.locals.pop().unwrap();

        if let Some(val) = locals.pop() {
            if val == lift {
                self.locals().push(lift)
            } else {
                locals.push(val)
            }
        }

        self.decrement_locals(&locals);
    }

    pub fn pop_locals_remove(&mut self, lift: BasicValueEnum) {
        let mut locals = self.locals.pop().unwrap();

        if let Some(val) = locals.pop() {
            if val != lift {
                locals.push(val)
            }
        }

        self.decrement_locals(&locals);
    }

    pub fn decrement_all_locals(&self) {
        for locals in &self.locals {
            self.decrement_locals(locals);
        }
    }

    fn decrement_locals(&self, locals: &[BasicValueEnum]) {
        if self.builder.get_insert_block().is_some() {
            for local in locals {
                self.decrement_refcount(*local);
            }
        }
    }

    pub fn build_phi(&mut self, nodes: &[(BasicValueEnum, BasicBlock)]) -> BasicValueEnum {
        let nodes = nodes.iter().filter(|(v, _)| v.get_type() != self.none_const.get_type()).collect::<Vec<_>>();

        match nodes.len() {
            0 => self.none_const,
            1 => nodes[0].0,
            _ => {
                let ty = nodes[0].0.get_type();
                let nodes = nodes.iter().map(|(v, b)| (v as &dyn BasicValue, b)).collect::<Vec<_>>();
                let phi = self.builder.build_phi(ty, "phi");
                phi.add_incoming(&nodes);
                self.locals().push(phi.as_basic_value());
                phi.as_basic_value()
            }
        }
    }
}
