mod evaluator;
mod asset;
mod parser;
mod utils;
mod mime;

use crate::utils::Result;
use anyhow::{anyhow, Context};
use clap::Parser;
use evaluator::Evaluator;
use shrimple_localhost::{Server, ServerError};
use std::{env::set_current_dir, path::PathBuf};

#[derive(Parser)]
#[command(name = "shrimple", author, version, about)]
struct Args {
    /// the output directory for all the files
    #[arg(long, short = 'o', default_value = "dist")]
    output: PathBuf,

    /// Locally serve & hot-reload the website
    #[arg(long, short = 'w')]
    watch: bool,

    /// Port at which the local server will be set up.
    /// Has no effect without the `-w/--watch` option.
    #[arg(long, short =  'p')]
    port: Option<u16>,

    /// The root of the website, it's `index.html`
    #[arg(default_value = "index.html")]
    file: PathBuf,
}

fn main() -> Result {
    let args = Args::parse();
    let output = args.output.canonicalize().context("failed to locate the output root")?;
    let abs_file = args.file.canonicalize().context("failed to locate the source file")?;
    let root = abs_file.parent().context("invalid source file")?;
    set_current_dir(root)?;
    if !args.watch {
        return Evaluator::default().eval_all(&abs_file, root, output)
    }

    println!("First build before setting up the server...");
    Evaluator::default().eval_all(&abs_file, root, &output)?;
    let port = args.port.unwrap_or(Server::DEFAULT_PORT);
    let mut server = Server::new_at(&output, port)?;
    let mut last_mtime = root.metadata()?.modified()?;
    println!("Local server with hot-reloading set-up at \"http://localhost:{port}\"");
    server.try_serve_with_callback(
        |_, _| {
            let mtime = root.metadata()?.modified()?;
            if mtime <= last_mtime {
                return Ok(())
            }
            last_mtime = mtime;
            println!("Change detected, rebuilding website...");
            Evaluator::default().eval_all(&abs_file, root, &output)
        },
        |addr, res| Ok(println!("Requested processed from {addr}, result: {res}")),
    )
    .map(|r| match r {})
    .map_err(|err| match err {
        ServerError::Io(err) => anyhow!(err),
        ServerError::Callback(err) => err,
    })
}
