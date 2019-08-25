/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 8/24/19 5:50 PM.
 * This file is under the GPL3 license. See LICENSE in the root directory of this repository for details.
 */

use super::mir::{MIRFunction, MIRType};
use crate::mir::mir::{MIRStruct, MIRVariable, MIRExpression};
use std::collections::HashMap;
use crate::mir::{MIR, MutRc, mutrc_new};
use std::rc::Rc;

pub struct MIRBuilder {
    position: Option<Pointer>,
    functions: HashMap<Rc<String>, MutRc<MIRFunction>>,
    types: HashMap<Rc<String>, MutRc<MIRStruct>>,
}

impl MIRBuilder {

    pub(super) fn create_struct(&mut self, name: Rc<String>) -> Option<MutRc<MIRStruct>> {
        let class = mutrc_new(MIRStruct {
            name: Rc::clone(&name),
            members: HashMap::new(),
            super_struct: None
        });

        if !self.types.contains_key(&name) {
            self.types.insert(Rc::clone(&name), Rc::clone(&class));
            Some(class)
        } else {
            // Struct already exists
            None
        }
    }

    pub(super) fn create_function(
        &mut self,
        name: Rc<String>,
        ret_type: MIRType,
        parameters: Vec<Rc<MIRVariable>>
    ) -> Option<MutRc<MIRFunction>> {
        let function = mutrc_new(MIRFunction {
            name: Rc::clone(&name),
            parameters,
            blocks: HashMap::new(),
            variables: HashMap::new(),
            ret_type
        });

        if !self.functions.contains_key(&name) {
            self.functions.insert(Rc::clone(&name), Rc::clone(&function));
            Some(function)
        } else {
            None
        }
    }

    /// Will create the variable in the current function.
    pub(super) fn add_function_variable(&mut self, variable: Rc<MIRVariable>) {
        let func = self.cur_fn();
        func.borrow_mut().variables.insert(Rc::clone(&variable.name), variable);
    }

    pub(super) fn build_store(&mut self, var: Rc<MIRVariable>, value: MIRExpression) -> MIRExpression {
        MIRExpression::VarStore {
            var,
            value: Box::new(value)
        }
    }

    pub(super) fn find_type(&self, name: &String) -> Option<MIRType> {
        Some(match &name[..] {
            "None" => MIRType::None,
            "bool" => MIRType::Bool,
            "i64" => MIRType::Int,
            "f64" => MIRType::Double,
            "String" => MIRType::String,
            _ => MIRType::Struct(self.find_struct(name)?)
        })
    }

    pub(super) fn find_struct(&self, name: &String) -> Option<MutRc<MIRStruct>> {
        Some(Rc::clone(self.types.get(name)?))
    }

    pub(super) fn find_function(&self, name: &String) -> Option<MutRc<MIRFunction>> {
        Some(Rc::clone(self.functions.get(name)?))
    }

    pub(super) fn set_pointer(&mut self, function: MutRc<MIRFunction>, block: Rc<String>) {
        self.position = Some(Pointer {
            function,
            block
        })
    }

    pub(super) fn insert_at_ptr(&mut self, expr: MIRExpression) {
        let func = self.cur_fn();
        let mut func = func.borrow_mut();
        func.blocks.get_mut(&self.position.as_ref().unwrap().block).unwrap().expressions.push(expr);
    }

    fn cur_fn(&self) -> MutRc<MIRFunction> {
        Rc::clone(&self.position.as_ref().unwrap().function)
    }

    pub(super) fn get_mir(self) -> MIR {
        MIR {
            types: self.types.into_iter().map(|(_, v)| v).collect(),
            functions: self.functions
        }
    }

    pub fn new() -> MIRBuilder {
        MIRBuilder {
            position: None,
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

pub struct Pointer {
    pub function: MutRc<MIRFunction>,
    block: Rc<String>,
}