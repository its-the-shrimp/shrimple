mod evaluator;
mod file_ref;
mod parser;
mod utils;
mod mime;

use crate::utils::Result;
use anyhow::Context;
use clap::Parser;
use evaluator::Evaluator;
use std::{env::set_current_dir, path::PathBuf};

#[derive(Parser)]
#[command(name = "shrimple", author, version, about)]
struct Args {
    /// the output directory for all the files
    #[arg(long, short = 'o')]
    output: PathBuf,

    file: PathBuf,
}

fn main() -> Result {
    let args = Args::parse();
    let output = args.output.canonicalize().context("failed to locate the output root")?;
    let abs_file = args.file.canonicalize().context("failed to locate the source file")?;
    let root = abs_file.parent().context("invalid source file")?;
    set_current_dir(root)?;
    Evaluator::default().eval(&abs_file, root, output)
}
