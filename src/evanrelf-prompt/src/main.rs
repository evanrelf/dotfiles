use camino::{Utf8Path, Utf8PathBuf};
use clap::Parser as _;
use std::{borrow::Cow, env, process};

#[derive(clap::Parser)]
enum Command {
    Prompt {
        /// Fish's `pwd` (oblivious to symlinks)
        #[arg(long)]
        pwd: Option<Utf8PathBuf>,

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
        Command::Prompt {
            pwd,
            pipestatus,
            jobs,
        } => run_prompt(pwd, pipestatus.as_deref(), jobs),
        Command::Init => run_init(),
    }
}

const RED: &str = "\x1b[31m";
const BRIGHT_BLUE: &str = "\x1b[94m";
const RESET: &str = "\x1b[0m";
const UNDERLINE: &str = "\x1b[4m";
const NO_UNDERLINE: &str = "\x1b[24m";

fn run_prompt(
    pwd: Option<Utf8PathBuf>,
    pipestatus: Option<&str>,
    jobs: Option<usize>,
) -> anyhow::Result<()> {
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
    let current_dir = match pwd {
        Some(pwd) => pwd,
        None => Utf8PathBuf::try_from(env::current_dir()?)?,
    };
    let repo = jj_root().ok();
    let pwd_display = match current_dir.strip_prefix(&home) {
        Ok(s) if s.as_str().is_empty() => "~".to_string(),
        Ok(s) => format!("~/{s}"),
        Err(_) => current_dir.as_str().to_string(),
    };

    let pwd = underline_repo(&pwd_display, &current_dir, repo.as_deref());

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

    let string = str::from_utf8(&output.stdout)?.trim();

    let path = Utf8PathBuf::from(string);

    Ok(path)
}

fn underline_repo<'a>(
    display_path: &'a str,
    current_dir: &Utf8Path,
    repo_root: Option<&Utf8Path>,
) -> Cow<'a, str> {
    let Some(repo_root) = repo_root else {
        return Cow::Borrowed(display_path);
    };

    if !current_dir.starts_with(repo_root) {
        return Cow::Borrowed(display_path);
    }

    let Some(repo_name) = repo_root.file_name() else {
        return Cow::Borrowed(display_path);
    };

    let mut result =
        String::with_capacity(display_path.len() + UNDERLINE.len() + NO_UNDERLINE.len());

    for component in display_path.split('/') {
        if !result.is_empty() {
            result.push('/');
        }
        if component == repo_name {
            result.push_str(UNDERLINE);
            result.push_str(component);
            result.push_str(NO_UNDERLINE);
        } else {
            result.push_str(component);
        }
    }

    Cow::Owned(result)
}

#[expect(clippy::unnecessary_wraps)]
fn run_init() -> anyhow::Result<()> {
    println!(
        "{}",
        r#"
function fish_prompt
    evanrelf-prompt prompt \
        --pwd (pwd) \
        --pipestatus "$pipestatus" \
        --jobs (jobs -g 2>/dev/null | count)
end
        "#
        .trim()
    );

    Ok(())
}
