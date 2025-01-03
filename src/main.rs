mod asset;
mod error;
mod escape_html;
mod evaluator;
mod mime;
mod parser;
mod utils;
mod view;

use crate::{
    evaluator::eval,
    utils::Result,
};
use anyhow::Context;
use clap::Parser;
use notify::{recommended_watcher, Config, RecursiveMode::Recursive, Watcher};
use shrimple_localhost::{print_request_result, Response, Server};
use std::{
    env::set_current_dir,
    fs::create_dir,
    io::ErrorKind,
    path::{Path, PathBuf},
    sync::{atomic::{AtomicBool, Ordering}, Arc},
};

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
    #[arg(long, short = 'p')]
    port: Option<u16>,

    /// Use manual polling to detect file changes.
    /// Has no effect without the `-w/--watch` option
    #[arg(long)]
    poll: bool,

    /// The root of the website, it's `index.html`
    #[arg(default_value = "index.html")]
    file: PathBuf,
}

static RECOMPILE: AtomicBool = AtomicBool::new(true);

fn main() -> Result {
    let args = Args::parse();
    let output: Arc<Path> = match args.output.canonicalize() {
        Err(e) if e.kind() == ErrorKind::NotFound => {
            create_dir(&args.output).context("failed to create the output directory")?;
            args.output.canonicalize()?
        }
        Err(e) => return Err(anyhow::Error::new(e).context("failed to locate the output root")),
        Ok(x) => x,
    }.into();
    let abs_file = args.file.canonicalize().context("failed to locate the source file")?;
    let root = abs_file.parent().context("invalid source file")?;
    set_current_dir(root)?;
    eval(&abs_file, root, &output)?;
    if !args.watch {
        return Ok(());
    }

    let port = args.port.unwrap_or(Server::DEFAULT_PORT);
    let mut server = Server::new_at(&output, port).context("failed to set up the local server")?;

    let mut watcher =
        recommended_watcher({
            let output = output.clone();
            move |event: notify::Result<notify::Event>| match event {
                Ok(event) => {
                    if event.paths.iter().any(|p| !p.starts_with(&output)) {
                        RECOMPILE.store(true, Ordering::Relaxed);
                    }
                }
                Err(err) => eprintln!("Error in the filesystem watcher:\n{err}"),
            }
        })
        .context("failed to configure the filesystem watcher")?;
    if args.poll {
        watcher
            .configure(Config::default().with_manual_polling())
            .context("failed to set the filesystem watcher to manual polling")?;
    }
    watcher.watch(root, Recursive).context("failed to start the filesystem watcher")?;

    println!("Local server with hot-reloading set up at http://127.0.0.1:{port}");
    let mut err_text = None;

    server
        .serve_with_callback(
            |_, path| {
                if !RECOMPILE.swap(false, Ordering::Relaxed) {
                    return err_text.clone().map_or_else(|| path.into(), Response::Data)
                }
                println!("Change detected, rebuilding website...");
                match eval(&abs_file, root, &output) {
                    Ok(_) => {
                        err_text = None;
                        path.into()
                    },
                    Err(e) => {
                        let mut text = e.to_string()
                            .replace('&', "&amp;")
                            .replace('<', "&lt;")
                            .replace('>', "&gt;")
                            .replace('"', "&quot;")
                            .replace('\'', "&apos;");
                        text.insert_str(0, "\
                            <!DOCTYPE html>\
                            <html>\
                                <head>\
                                    <meta charset=\"UTF-8\" />\
                                </head>\
                                <body style=\"background-color: black; color: #CC2222\">\
                                    <code><pre>shrimple: compilation failed\n");
                        text.push_str("</pre></code></body></html>");
                        err_text = Some(text.clone().into_bytes());
                        Response::Data(text.into_bytes())
                    }
                }
            },
            print_request_result,
        )?;

    Ok(())
}
