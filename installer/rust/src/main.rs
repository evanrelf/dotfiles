use ifmt::iformat;
use once_cell::sync::OnceCell;
use std::path::Path;
use std::process::{Command, Stdio};
use structopt::StructOpt;

static DRY_RUN: OnceCell<bool> = OnceCell::new();

fn main() {
    let options = Options::from_args();

    DRY_RUN.set(options.dry_run).unwrap();

    let all_packages = options
        .packages
        .iter()
        .map(|p| p.chars().filter(|c| *c != '/').collect())
        .collect();

    let existent_packages = discard_nonexistent(all_packages);

    if existent_packages.is_empty() {
        println_colored(ansi_term::Color::Red, "No packages specified".to_string());
        std::process::exit(1);
    } else {
        for package in existent_packages {
            install(package);
        }
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "installer")]
struct Options {
    #[structopt(name = "PACKAGE")]
    packages: Vec<String>,

    /// Run in dry run mode
    #[structopt(long)]
    dry_run: bool,
}

fn discard_nonexistent(packages: Vec<String>) -> Vec<String> {
    let mut existent = vec![];

    for package in packages {
        if Path::new(&package).exists() {
            existent.push(package);
        } else {
            println_colored(
                ansi_term::Color::Yellow,
                iformat!("[{package}] Configuration doesn't exist"),
            );
        }
    }

    existent
}

fn install(package: String) {
    run_hook("before".to_string(), package.clone());
    stow(package.clone());
    run_hook("after".to_string(), package.clone());
}

fn run_hook(hook_name: String, package: String) {
    let script = iformat!("{package}/{hook_name}-hook");

    if Path::new(&script).exists() {
        sh(iformat!("./{script}"));
    }
}

fn stow(package: String) {
    log(iformat!("[{package}] Stowing configuration"));

    sh(iformat!(concat![
        "stow",
        "--stow",
        "--stow",
        "--target \"${HOME}\"",
        "--no-folding {package}",
        "--ignore \"-hook\"",
    ]));
}

fn sh(command: String) {
    if *DRY_RUN.get().expect("dry_run not initialized") {
        log(iformat!("dry-run> {command}"));
    } else {
        log(iformat!("+ {command}"));
        Command::new("sh")
            .args(&["-c", command.as_str()])
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to run command");
    }
}

fn log(message: String) {
    println_colored(ansi_term::Color::Purple, message);
}

fn println_colored(color: ansi_term::Color, message: String) {
    println!("{}", color.paint(message));
}
