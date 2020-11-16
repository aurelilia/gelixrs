/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 2/3/20 2:26 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::{env, fs, path::PathBuf, process};

use gelixrs::{find_std_module, stem_to_smol};
use inkwell::{execution_engine::JitFunction, OptimizationLevel};
use structopt::StructOpt;

#[derive(StructOpt, Debug, Default)]
#[structopt(name = "gelixrs", about = "A compiler for the gelix language.")]
struct Opt {
    /// Run in-place instead of compiling
    #[structopt(short, long)]
    run: bool,

    /// Parse to AST and exit
    #[structopt(long = "parse")]
    parse: bool,

    /// Compile to GIR, print, and exit
    #[structopt(long)]
    gir: bool,

    /// Compile to GIR, print including all libs, and exit
    #[structopt(long)]
    gir_all: bool,

    /// Compile to LLVM IR, print, and exit
    #[structopt(long)]
    ir: bool,

    /// Don't auto-import 'std/prelude' into every module
    #[structopt(long = "no-prelude")]
    no_prelude: bool,

    /// Path of the resulting executable
    #[structopt(short, long)]
    output: Option<PathBuf>,

    /// The level of optimization to use with clang
    #[structopt(short = "O", default_value = "3")]
    optimize_level: usize,

    /// File to compile
    #[structopt(parse(from_os_str))]
    file: PathBuf,
}

fn main() {
    run(Opt::from_args()).map_err(|e| println!("{}", e)).ok();
}

fn run(args: Opt) -> Result<(), &'static str> {
    if !args.file.exists() {
        return Err("Given path does not exist.");
    }

    let _std_mod = find_std_module()?;
    let modules = vec![args.file.clone()];

    let mut code = gelixrs::parse_source(modules).map_err(|errors| {
        for file in errors {
            println!("{} error(s):\n{}", file.0.len(), file);
            println!();
        }
        "Parser encountered errors. Exiting."
    })?;

    if !args.no_prelude {
        gelixrs::auto_import_prelude(&mut code)
    }

    if args.parse {
        let stem = stem_to_smol(&args.file);
        for module in code.iter().filter(|m| m.path.0.first().unwrap() == &stem) {
            println!("{:#?}\n\n", module);
        }
        return Ok(());
    }

    let gir = gelixrs::compile_gir(code).map_err(|errors| {
        for error in errors {
            println!("{}\n", error);
        }
        "GIR generator encountered errors. Exiting."
    })?;

    if args.gir || args.gir_all {
        let stem = stem_to_smol(&args.file);
        for module in gir
            .iter()
            .filter(|m| (m.borrow().path.0.first().unwrap() == &stem) || args.gir_all)
        {
            println!("{}", module.borrow())
        }
        return Ok(());
    }

    let module = gelixrs::compile_ir(gir);

    if args.ir {
        match args.output {
            Some(file) => {
                module.print_to_file(file).ok();
            }
            None => module.print_to_stderr(),
        };
        return Ok(());
    }

    if args.run {
        let engine = module
            .create_jit_execution_engine(OptimizationLevel::Default)
            .map_err(|_| "Failed to create JIT VM.")?;

        unsafe {
            let main_fn: JitFunction<unsafe extern "C" fn()> = engine
                .get_function("main")
                .map_err(|_| "No main fn in JIT?")?;
            main_fn.call();
        }

        return Ok(());
    }

    let mut tmp_dir = env::temp_dir();
    tmp_dir.push("gelixrs");
    if !tmp_dir.exists() {
        fs::create_dir(&tmp_dir).expect("Failed to create temporary directory!");
    }

    let mut module_file = tmp_dir;
    module_file.push("out.bc");
    module.write_bitcode_to_path(&module_file);

    if args.optimize_level > 3 {
        return Err("Invalid optimize level.");
    }
    let status = process::Command::new("clang")
        .arg("-o")
        .arg(&args.output.ok_or("Output location required.")?)
        .arg(module_file)
        .arg(format!("-O{}", args.optimize_level))
        .output()
        .expect("Evoking clang failed.")
        .status;

    if status.success() {
        println!("Compilation successful!");
        Ok(())
    } else {
        Err("Compiling to native binary failed. Please file a bug report.")
    }
}

#[cfg(test)]
mod tests {
    use crate::{run, Opt};
    use std::{env, path::PathBuf};

    fn get_test(name: &'static str) -> PathBuf {
        let mut test_path = env::current_dir().expect("Couldn't get current dir.");
        test_path.push("tests");
        test_path.push(name);
        test_path
    }

    #[test]
    #[ignore]
    fn unknown_path() {
        assert!(
            run(Opt {
                parse: true,
                file: get_test("who.gel"),
                ..Default::default()
            }) == Err("Given path does not exist.")
        )
    }

    #[test]
    #[ignore]
    fn parse_err() {
        assert!(
            run(Opt {
                parse: true,
                file: get_test("unexpected_character.gel"),
                ..Default::default()
            }) == Err("Parser encountered errors. Exiting.")
        )
    }

    #[test]
    #[ignore]
    fn compile_err() {
        assert!(
            run(Opt {
                file: get_test("empty_file.gel"),
                ..Default::default()
            }) == Err("GIR generator encountered errors. Exiting.")
        )
    }

    #[test]
    #[ignore]
    fn no_prelude() {
        assert!(
            run(Opt {
                no_prelude: true,
                file: get_test("scoping.gel"),
                ..Default::default()
            }) == Err("GIR generator encountered errors. Exiting.")
        )
    }

    #[test]
    #[ignore]
    fn parse_only() -> Result<(), &'static str> {
        run(Opt {
            parse: true,
            file: get_test("unicode.gel"),
            ..Default::default()
        })
    }

    #[test]
    #[ignore]
    fn gir_only() -> Result<(), &'static str> {
        run(Opt {
            gir: true,
            file: get_test("unicode.gel"),
            ..Default::default()
        })
    }

    #[test]
    #[ignore]
    fn ir_only() -> Result<(), &'static str> {
        run(Opt {
            ir: true,
            file: get_test("unicode.gel"),
            ..Default::default()
        })
    }

    #[test]
    #[ignore]
    fn missing_output() {
        assert!(
            run(Opt {
                file: get_test("unicode.gel"),
                ..Default::default()
            }) == Err("Output location required.")
        )
    }

    #[test]
    #[ignore]
    fn normal_compile() {
        run(Opt {
            file: get_test("unicode.gel"),
            output: Some(PathBuf::from("/tmp/gelix-test")),
            ..Default::default()
        })
        .ok()
        .unwrap();
        assert!(PathBuf::from("/tmp/gelix-test").exists())
    }
}
