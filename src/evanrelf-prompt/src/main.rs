use camino::Utf8PathBuf;
use clap::Parser as _;
use std::env;

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
        Some(pipestatus) => &format!("{}\n", pipestatus.replace(' ', " | ")),
    };

    // TODO: Underline path component for repo.
    // `jj --at-operation @ --ignore-working-copy root`
    let home = Utf8PathBuf::try_from(env::home_dir().unwrap())?;
    let current_dir = Utf8PathBuf::try_from(env::current_dir()?)?;
    let pwd = match current_dir.strip_prefix(&home) {
        Ok(pwd) if pwd.as_str().is_empty() => "~",
        Ok(pwd) => &format!("~/{pwd}"),
        Err(_) => current_dir.as_str(),
    };

    let in_nix_shell = match env::var("IN_NIX_SHELL") {
        Ok(_) => "  ",
        Err(_) => "",
    };

    let jobs = if jobs.unwrap_or(0) >= 1 { "  " } else { "" };

    print!("\n{RED}{status}{BRIGHT_BLUE}{pwd}{in_nix_shell}{jobs}\n${RESET} ");

    Ok(())
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
