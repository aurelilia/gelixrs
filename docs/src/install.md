# Installation

### Dependencies

Before installing `gelixrs`, please ensure you have the following dependencies 
installed on your system:
- The `clang` compiler
- The LLVM library (8.0 recommended, others might work by changing version in `Cargo.toml`)
- A nightly rust compiler

### Downloading the source

Start by cloning the repository:

``` git clone https://gitea.angm.xyz/ellie/gelixrs.git ```

Alternatively, head to the [repo](https://gitea.angm.xyz/ellie/gelixrs)
in your browser and download a ZIP of the archive.

### Compiling and installing

To compile and install the compiler on Linux, simply run:

```bash
# Compile with optimizations
cargo build --release

# Copy to a location in your $PATH:
cp ./target/release/gelixrs ~/.local/bin/gelix
# Alternatively, to install system-wide:
sudo cp ./target/release/gelixrs /usr/local/bin/gelix

# Also copy the standard library to its path:
mkdir ~/.local/share/gelix
cp std ~/.local/share/gelix/
# Alternatively, to install system-wide:
sudo mkdir /usr/local/lib/gelix/
sudo cp std /usr/local/lib/gelix/
```

You should now be able to invoke the compiler with `gelix`.

Should you use Mac OS or Windows, you'll have to put the compiler into a location
in your PATH, and put the standard library into 
`C:\Users\...\AppData\Roaming\gelix\std` or `/Users/.../Library/Application Support/gelix/std` respectively.