mod utils;
mod error;
mod parser;

use std::{fs::read_to_string, path::PathBuf};
use clap::Parser;
use parser::parse_file;
use utils::Result;

#[derive(Parser)]
#[command(name = "shrimple", author, version, about)]
struct Cli {
    files: Vec<PathBuf>,
}

fn main() -> Result {
    let cli = Cli::parse();
    for path in cli.files {
        let contents = read_to_string(&path)?;
        println!("{}:\n{:#?}", path.display(), parse_file(&path, &contents)?)
    }
    Ok(())
}
