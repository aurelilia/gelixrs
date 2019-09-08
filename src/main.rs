/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 9/8/19, 6:09 PM.
 * This file is under the GPL3 license. See LICENSE in the root directory of this repository for details.
 */

use std::{env, fs, path::PathBuf, process};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "gelixrs")]
struct Opt {
    /// Run in-place instead of compiling
    #[structopt(short, long)]
    run: bool,

    /// Parse to AST and exit
    #[structopt(long = "parse-only")]
    parse_only: bool,

    /// Compile to LLVM IR, print, and exit
    #[structopt(long)]
    ir: bool,

    /// Path of the resulting executable
    #[structopt(short, long)]
    output: Option<PathBuf>,

    /// File to compile
    #[structopt(parse(from_os_str))]
    file: PathBuf,
}

fn main() -> Result<(), &'static str> {
    let args = Opt::from_args();

    if !args.file.exists() {
        return Err("Given path does not exist.");
    }

    let code = gelixrs::parse_source(args.file).or_else(|errors| {
        for file in errors {
            println!(
                "{} error(s) in file {}:\n",
                file.errors.len(),
                file.file_name
            );
            for error in file.errors {
                println!("{}\n", error.to_string(&file.source));
            }
            println!();
        }
        Err("Parser encountered errors. Exiting.")
    })?;

    if args.parse_only {
        println!("{:#?}", code);
        return Ok(());
    }

    let module = gelixrs::compile_ir(code).or_else(|_error| {
        // println!("{}", error.to_string(&source)); todo
        Err("MIR generator encountered errors. Exiting.")
    })?;

    if args.ir {
        module.print_to_stderr();
        return Ok(());
    }

    let mut tmp_dir = env::temp_dir();
    tmp_dir.push("gelixrs");
    if !tmp_dir.exists() {
        fs::create_dir(&tmp_dir).expect("Failed to create temporary directory!");
    }

    let mut module_file = tmp_dir.clone();
    module_file.push("out.bc");
    module.write_bitcode_to_path(&module_file);

    let mut asm_file = tmp_dir;
    asm_file.push("out.o");

    let mut stdlib_dir = env::current_dir().expect("Failed to get current directory!");
    stdlib_dir.push("stdlib");
    stdlib_dir.push("target");
    stdlib_dir.push("x86_64-unknown-linux-musl");
    stdlib_dir.push("release");

    // TODO: Invoking clang as a command feels soo wrong, but I don't think there's a stable Rust API for it...
    process::Command::new("clang")
        .arg("-c")
        .arg("-o")
        .arg(&asm_file)
        .arg(module_file)
        .status()
        .expect("Evoking clang failed.");

    let status = process::Command::new("clang")
        .arg("-static")
        .arg("-o")
        .arg(&args.output.ok_or("Output location required.")?)
        .arg(asm_file)
        .arg("-L")
        .arg(stdlib_dir)
        .arg("-lstdlib")
        .status()
        .expect("Evoking clang failed.");

    if status.success() {
        println!("Compilation successful!");
        Ok(())
    } else {
        Err("Compiling to native binary failed. Please file a bug report.")
    }
}
