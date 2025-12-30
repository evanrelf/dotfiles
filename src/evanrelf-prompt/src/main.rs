use camino::Utf8PathBuf;
use clap::Parser as _;
use std::{env, process};

#[derive(clap::Parser)]
enum Command {
    Prompt {
        /// Fish's `$pipestatus` variable
        #[arg(long)]
        pipestatus: Option<String>,

        /// Job count
        #[arg(long)]
        jobs: Option<usize>,
    },
    Init,
}

fn main() -> anyhow::Result<()> {
    match Command::parse() {
        Command::Prompt { pipestatus, jobs } => run_prompt(pipestatus.as_deref(), jobs),
        Command::Init => run_init(),
    }
}

const RED: &str = "\x1b[31m";
const BRIGHT_BLUE: &str = "\x1b[94m";
const RESET: &str = "\x1b[0m";

fn run_prompt(pipestatus: Option<&str>, jobs: Option<usize>) -> anyhow::Result<()> {
    let status = match pipestatus.map(str::trim) {
        Some("0") | None => "",
        Some(pipestatus) if pipestatus.split(' ').all(|s| s == "0") => "",
        Some(pipestatus) => &format!("{}\n", pipestatus.replace(' ', " | ")),
    };

    let hostname = match nix::unistd::gethostname()?.into_string() {
        Ok(s) => match s.strip_suffix(".local") {
            Some(s) => String::from(s),
            None => s,
        },
        Err(_) => anyhow::bail!("Failed to convert hostname to UTF-8 string"),
    };

    let home = Utf8PathBuf::try_from(env::home_dir().unwrap())?;
    let current_dir = Utf8PathBuf::try_from(env::current_dir()?)?;
    let repo = jj_root();
    // TODO: Underline path component for repo.
    let pwd = match current_dir.strip_prefix(&home) {
        Ok(s) if s.as_str().is_empty() => "~",
        Ok(s) => &format!("~/{s}"),
        Err(_) => current_dir.as_str(),
    };

    let in_nix_shell = match env::var("IN_NIX_SHELL") {
        Ok(_) => "  ",
        Err(_) => "",
    };

    let jobs = if jobs.unwrap_or(0) >= 1 { "  " } else { "" };

    print!("\n{RED}{status}{BRIGHT_BLUE}{hostname}:{pwd}{in_nix_shell}{jobs}\n${RESET} ");

    Ok(())
}

fn jj_root() -> anyhow::Result<Utf8PathBuf> {
    let output = process::Command::new("jj")
        .arg("--at-operation=@")
        .arg("--ignore-working-copy")
        .arg("root")
        .output()?;

    if !output.status.success() {
        anyhow::bail!(
            "jj root failed:\n{}",
            str::from_utf8(&output.stderr).unwrap_or("<error: stderr not utf8>")
        );
    }

    let string = str::from_utf8(&output.stdout)?;

    let path = Utf8PathBuf::from(string);

    Ok(path)
}

#[expect(clippy::unnecessary_wraps)]
fn run_init() -> anyhow::Result<()> {
    println!(
        "{}",
        r#"
function fish_prompt
    evanrelf-prompt prompt \
        --pipestatus "$pipestatus" \
        --jobs (jobs -g 2>/dev/null | count)
end
        "#
        .trim()
    );

    Ok(())
}
