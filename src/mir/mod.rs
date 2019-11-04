/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 11/4/19 7:55 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Error, Formatter};
use std::rc::Rc;

use nodes::{Class, Variable};

use crate::ast::declaration::Type;
use crate::lexer::token::Token;
use crate::mir::generator::{MIRError, MIRGenerator};
use crate::mir::nodes::Interface;
use crate::option::Flatten;
use crate::{module_path_to_string, ModulePath};

pub mod generator;
pub mod nodes;

pub type MutRc<T> = Rc<RefCell<T>>;

fn mutrc_new<T>(value: T) -> MutRc<T> {
    Rc::new(RefCell::new(value))
}

/// A struct produced by a generator. It contains the full MIR representation of a file/module.
#[derive(Debug, Default)]
pub struct MIRModule {
    /// The path of the module, for example my_app/gui/widgets
    /// Mainly used for namespacing in IR.
    pub path: Rc<ModulePath>,
    /// All classes in this module.
    pub classes: HashMap<Rc<String>, MutRc<Class>>,
    /// All interfaces.
    pub interfaces: HashMap<Rc<String>, MutRc<Interface>>,
    /// All functions.
    pub functions: HashMap<Rc<String>, Rc<Variable>>,
}

impl MIRModule {
    pub fn new(path: Rc<ModulePath>) -> MIRModule {
        Self {
            path,
            ..Default::default()
        }
    }
}

impl Display for MIRModule {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "----------------------------------------")?;
        writeln!(f, "Module {}", module_path_to_string(&self.path))?;
        writeln!(f, "----------------------------------------\n")?;

        if !self.functions.is_empty() {
            writeln!(f, "---------- Functions ----------")?;
        }
        for func in self
            .functions
            .iter()
            .map(|f| MIRGenerator::var_to_function(f.1))
        {
            writeln!(f, "{}", func.borrow())?;
        }

        if !self.classes.is_empty() {
            writeln!(f, "---------- Classes ----------")?;
        }
        for (_, class) in self.classes.iter() {
            writeln!(f, "{}", class.borrow())?;
        }

        Ok(())
    }
}

pub trait ToMIRResult<T> {
    fn or_err(self, gen: &MIRGenerator, error_token: &Token, msg: &str) -> Result<T, MIRError>;

    fn or_anon_err(
        self,
        gen: &MIRGenerator,
        error_token: Option<&Token>,
        msg: &str,
    ) -> Result<T, MIRError>;

    fn or_type_err(
        self,
        gen: &MIRGenerator,
        error_ty: &Option<Type>,
        msg: &str,
    ) -> Result<T, MIRError>;
}

impl<T> ToMIRResult<T> for Option<T> {
    #[inline(always)]
    fn or_err(self, gen: &MIRGenerator, error_token: &Token, msg: &str) -> Result<T, MIRError> {
        self.ok_or_else(|| gen.error(error_token, error_token, msg))
    }

    #[inline(always)]
    fn or_anon_err(
        self,
        gen: &MIRGenerator,
        error_token: Option<&Token>,
        msg: &str,
    ) -> Result<T, MIRError> {
        self.ok_or_else(|| gen.anon_err(error_token, msg))
    }

    #[inline(always)]
    fn or_type_err(
        self,
        gen: &MIRGenerator,
        error_ty: &Option<Type>,
        msg: &str,
    ) -> Result<T, MIRError> {
        self.ok_or_else(|| gen.anon_err(error_ty.as_ref().map(|t| t.get_token()).flatten_(), msg))
    }
}
