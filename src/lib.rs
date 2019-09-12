/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 9/12/19, 5:22 PM.
 * This file is under the GPL3 license. See LICENSE in the root directory of this repository for details.
 */

// https://github.com/rust-lang/rust/pull/63059
#![feature(bind_by_move_pattern_guards)]
// https://github.com/rust-lang/rust/pull/60256
#![feature(option_flattening)]
// https://github.com/rust-lang/rust/issues/43244
#![feature(drain_filter)]

#[macro_use]
#[cfg(test)]
extern crate lazy_static;

pub mod ast;
pub mod error;
pub mod ir;
pub mod lexer;
pub mod mir;
pub mod parser;
#[cfg(test)]
pub mod tests;

use mir::generator::module::MIRModuleGenerator;
use mir::generator::MIRError;
use parser::ParserErrors;
use ast::module::Module;
use error::Error;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;
use mir::MIRModule;
use ir::IRGenerator;

type ModulePath = Vec<Rc<String>>;
type SrcParseErrors = Vec<ParserErrors>;

pub fn module_path_to_string(path: &ModulePath) -> String {
    path
        .iter()
        .map(|rc| (&**rc).clone())
        .collect::<Vec<String>>()
        .join("/")
}

pub fn parse_source(input: Vec<PathBuf>) -> Result<Vec<Module>, SrcParseErrors> {
    let mut modules = Vec::new();
    for path in input {
        make_modules(path, &mut vec![], &mut modules)?;
    }
    Ok(modules)
}

fn make_modules(
    input: PathBuf,
    path: &mut ModulePath,
    modules: &mut Vec<Module>,
) -> Result<(), SrcParseErrors> {
    path.push(stem_to_rc_str(&input));

    if let Ok(dir) = input.read_dir() {
        let mut errors = Vec::new();
        for file in dir {
            let file = file.expect("Failed to read file").path();

            // If the file is named 'module.gel', it should have the
            // containing directory as its module path.
            let result = if file.file_name().unwrap() == "module.gel" {
                parse_module(file, path).map(|m| modules.push(m))
            } else {
                make_modules(file, path, modules)
            };

            if let Err(mut errs) = result {
                errors.append(&mut errs);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }
    } else if *input
        .extension()
        .map(|ext| ext == "gel")
        .get_or_insert(false)
    {
        // If 'input' is a .gel file; parse it if true
        modules.push(parse_module(input, path)?);
    }

    path.pop();
    Ok(())
}

fn parse_module(input: PathBuf, path: &mut ModulePath) -> Result<Module, SrcParseErrors> {
    let code = fs::read_to_string(&input).expect("Failed to read file.");
    let mut module = Module::new(path);

    fill_module(&code, &mut module).map_err(|err| vec![ParserErrors::new(err, &code, path)])?;
    Ok(module)
}

fn fill_module(code: &str, module: &mut Module) -> Result<(), Vec<Error>> {
    let lexer = lexer::Lexer::new(code);
    let parser = parser::Parser::new(lexer);
    parser.parse(module)
}

pub fn compile_mir(modules: Vec<Module>) -> Result<Vec<MIRModule>, Vec<MIRError>> {
    let pool = MIRModuleGenerator::new(modules);
    pool.execute()
}

pub fn compile_ir(modules: Vec<MIRModule>) -> inkwell::module::Module {
    let gen = IRGenerator::new();
    gen.generate(modules)
}

fn stem_to_rc_str(path: &PathBuf) -> Rc<String> {
    Rc::new(path.file_stem().unwrap().to_str().unwrap().to_string())
}
