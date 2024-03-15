#![recursion_limit = "512"]

mod error;
mod evaluator;
mod parser;
mod utils;

use crate::error::Locator;
use crate::evaluator::Evaluator;
use crate::utils::Result;
use clap::Parser;
use std::{fs::read_to_string, path::PathBuf};

#[derive(Parser)]
#[command(name = "shrimple", author, version, about)]
struct Cli {
    files: Vec<PathBuf>,
}

fn main() -> Result {
    let cli = Cli::parse();
    let mut evaluator = Evaluator::default();
    let mut dst = String::new();
    let contents = cli
        .files
        .into_iter()
        .map(|f| anyhow::Ok((read_to_string(&f)?, f)))
        .collect::<Result<Vec<_>, _>>()?;
    for (contents, filename) in &contents {
        let err_ctx = Locator::new(contents, filename);
        evaluator.eval(contents, &mut dst, &err_ctx)?;
        println!("{}:\n{}", filename.display(), dst)
    }
    Ok(())
}
