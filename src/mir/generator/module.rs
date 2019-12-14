/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/5/19 5:11 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::rc::Rc;

use crate::ast::module::Module;
use crate::mir::generator::intrinsics::INTRINSICS;
use crate::mir::generator::passes::declare_class::declare_class_pass;
use crate::mir::generator::passes::declare_func::declare_func_pass;
use crate::mir::generator::passes::declare_interface::declare_interface_pass;
use crate::mir::generator::passes::fill_class::fill_class_pass;
use crate::mir::generator::passes::iface_impl::iface_impl_pass;
use crate::mir::generator::passes::import::{
    class_imports, ensure_no_imports, function_imports, interface_imports,
};
use crate::mir::generator::{MIRError, MIRGenerator, Res};
use crate::mir::MModule;
use crate::mir::IFACE_IMPLS;

/// A set of [MIRGenerator]s.
/// Takes a list of module ASTs and transforms them into
/// MIR modules.
pub struct MIRModuleGenerator {
    modules: Vec<(Module, MIRGenerator)>,
}

impl MIRModuleGenerator {
    pub fn execute(mut self) -> Result<Vec<MModule>, Vec<MIRError>> {
        reset_mir();
        self.run_for_all(&declare_class_pass)?;
        class_imports(&mut self.modules)?;
        self.run_for_all(&declare_interface_pass)?;
        interface_imports(&mut self.modules)?;
        self.run_for_all(&declare_func_pass)?;
        function_imports(&mut self.modules)?;
        ensure_no_imports(&mut self.modules)?;
        self.run_for_all(&iface_impl_pass)?;
        INTRINSICS.with(|i| i.borrow_mut().populate(&mut self.modules));
        self.run_for_all(&fill_class_pass)?;

        let modules = self
            .modules
            .into_iter()
            .map(|(module, mut gen)| {
                gen.generate_mir(&module)?;
                Ok(gen.builder.consume_module())
            })
            .collect::<Result<Vec<MModule>, MIRError>>()
            .map_err(|e| vec![e]);

        INTRINSICS.with(|i| i.borrow_mut().validate())?;
        modules
    }

    fn run_for_all(
        &mut self,
        func: &'static dyn Fn(&mut MIRGenerator, &mut Module) -> Res<()>,
    ) -> Result<(), Vec<MIRError>> {
        let mut errors = Vec::new();
        for (module, gen) in self.modules.iter_mut() {
            let result = func(gen, module);
            if let Err(err) = result {
                errors.push(err)
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn new(modules: Vec<Module>) -> Self {
        let modules = modules.into_iter().map(|module| {
            let path_clone = Rc::clone(&module.path);
            (module, MIRGenerator::new(path_clone))
        });

        Self {
            modules: modules.collect(),
        }
    }
}

/// This function resets MIR global state.
/// Called before starting a new compile.
fn reset_mir() {
    INTRINSICS.with(|i| i.borrow_mut().reset());
    IFACE_IMPLS.with(|i| i.borrow_mut().clear());
}
