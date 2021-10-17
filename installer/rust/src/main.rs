use once_cell::sync::OnceCell;
use std::{
    path::Path,
    process::{Command, Stdio},
};
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
        println_colored(ansi_term::Color::Red, "No packages specified");
        std::process::exit(1);
    } else {
        for package in existent_packages {
            install(&package);
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
                &format!("[{}] Configuration doesn't exist", package),
            );
        }
    }

    existent
}

fn install(package: &str) {
    run_hook("before", package);
    stow(package);
    run_hook("after", package);
}

fn run_hook(hook_name: &str, package: &str) {
    let script = format!("{}/{}-hook", package, hook_name);

    if Path::new(&script).exists() {
        sh(&format!("./{}", script));
    }
}

fn stow(package: &str) {
    log(&format!("[{}] Stowing configuration", package));

    sh(&format!(
        concat![
            "stow",
            "--stow",
            "--target \"${{HOME}}\"",
            "--no-folding {}",
            "--ignore \"-hook\"",
        ],
        package
    ));
}

fn sh(command: &str) {
    if *DRY_RUN.get().expect("dry_run not initialized") {
        log(&format!("dry-run> {}", command));
    } else {
        log(&format!("+ {}", command));
        Command::new("sh")
            .args(&["-c", command])
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to run command");
    }
}

fn log(message: &str) {
    println_colored(ansi_term::Color::Purple, message);
}

fn println_colored(color: ansi_term::Color, message: &str) {
    println!("{}", color.paint(message));
}
