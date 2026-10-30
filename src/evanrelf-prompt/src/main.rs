use anyhow::Context as _;
use camino::{Utf8Path, Utf8PathBuf};
use clap::Parser as _;
use std::{borrow::Cow, env, process};

#[derive(clap::Parser)]
enum Command {
    Prompt(PromptArgs),
    Hostname,
    Init,
}

#[derive(clap::Args)]
struct PromptArgs {
    #[arg(long)]
    pipestatus: Option<String>,

    #[arg(long)]
    pwd: Option<Utf8PathBuf>,

    #[arg(long)]
    jobs: Option<usize>,

    #[arg(long)]
    hostname: Option<String>,

    #[arg(long)]
    final_rendering: bool,

    #[arg(allow_hyphen_values = true)]
    unknown: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    match Command::parse() {
        Command::Prompt(args) => run_prompt(args),
        Command::Hostname => run_hostname(),
        Command::Init => run_init(),
    }
}

const RED: &str = "\x1b[31m";
const WHITE: &str = "\x1b[37m";
const RESET: &str = "\x1b[0m";
const UNDERLINE: &str = "\x1b[4m";
const NO_UNDERLINE: &str = "\x1b[24m";

fn run_prompt(args: PromptArgs) -> anyhow::Result<()> {
    let status = match args.pipestatus.as_deref().map(str::trim) {
        Some("0") | None => "",
        Some(pipestatus) if pipestatus.split(' ').all(|s| s == "0") => "",
        Some(pipestatus) => &format!("{}\n", pipestatus.replace(' ', " | ")),
    };

    let hostname = match args.hostname.filter(|s| !s.is_empty()) {
        Some(hostname) => hostname,
        None => get_hostname()?,
    };

    let home = Utf8PathBuf::try_from(env::home_dir().context("Failed to get home directory")?)
        .context("Home directory path is not UTF-8")?;
    let current_dir = match args.pwd {
        Some(pwd) => pwd,
        None => {
            Utf8PathBuf::try_from(env::current_dir().context("Failed to get current directory")?)
                .context("Current directory path is not UTF-8")?
        }
    };
    let repo = get_repo_root(&current_dir);
    let pwd_display = match current_dir.strip_prefix(&home) {
        Ok(s) if s.as_str().is_empty() => "~".to_string(),
        Ok(s) => format!("~/{s}"),
        Err(_) => current_dir.as_str().to_string(),
    };

    let pwd = underline_repo(&pwd_display, &current_dir, repo.as_deref());

    let in_nix_shell = if !args.final_rendering && env::var("IN_NIX_SHELL").is_ok() {
        "  "
    } else {
        ""
    };

    let jobs = if !args.final_rendering && args.jobs.unwrap_or(0) >= 1 {
        "  "
    } else {
        ""
    };

    print!("\n{RED}{status}{WHITE}{hostname} {pwd}{in_nix_shell}{jobs}\n${RESET} ");

    Ok(())
}

fn run_hostname() -> anyhow::Result<()> {
    println!("{}", get_hostname()?);
    Ok(())
}

fn get_hostname() -> anyhow::Result<String> {
    if env::consts::OS == "macos" {
        get_macos_computer_name().context("Failed to get macOS computer name")
    } else {
        get_unix_hostname().context("Failed to get Unix hostname")
    }
}

fn get_unix_hostname() -> anyhow::Result<String> {
    let Ok(string) = nix::unistd::gethostname()?.into_string() else {
        anyhow::bail!("Failed to convert hostname to UTF-8 string")
    };
    Ok(string.strip_suffix(".local").unwrap_or(&string).to_owned())
}

fn get_macos_computer_name() -> anyhow::Result<String> {
    let output = process::Command::new("/usr/sbin/scutil")
        .arg("--get=ComputerName")
        .output()
        .context("Failed to get output of `scutil`")?;

    if !output.status.success() {
        anyhow::bail!(
            "scutil --get=ComputerName failed:\n{}",
            str::from_utf8(&output.stderr).unwrap_or("<error: stderr not utf8>")
        );
    }

    let name = String::from(
        str::from_utf8(&output.stdout)
            .context("`scutil` output is not UTF-8")?
            .trim(),
    );

    Ok(name)
}

fn get_repo_root(current_dir: &Utf8Path) -> Option<Utf8PathBuf> {
    // `.git` is a file in worktrees and submodules, so check `exists` rather than `is_dir`.
    current_dir
        .ancestors()
        .find(|dir| dir.join(".jj").is_dir() || dir.join(".git").exists())
        .map(Utf8Path::to_path_buf)
}

fn underline_repo<'a>(
    display_path: &'a str,
    current_dir: &Utf8Path,
    repo_root: Option<&Utf8Path>,
) -> Cow<'a, str> {
    let Some(repo_root) = repo_root else {
        return Cow::Borrowed(display_path);
    };

    let Ok(relative_dir) = current_dir.strip_prefix(repo_root) else {
        return Cow::Borrowed(display_path);
    };

    let Some(repo_name) = repo_root.file_name() else {
        return Cow::Borrowed(display_path);
    };

    // Locate the repo root's component by counting back from the end, so other components with the
    // same name aren't underlined.
    let Some(repo_index) = display_path
        .split('/')
        .count()
        .checked_sub(relative_dir.components().count() + 1)
    else {
        return Cow::Borrowed(display_path);
    };

    let mut result =
        String::with_capacity(display_path.len() + UNDERLINE.len() + NO_UNDERLINE.len());

    for (i, component) in display_path.split('/').enumerate() {
        if i > 0 {
            result.push('/');
        }
        if i == repo_index && component == repo_name {
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
set --global fish_transient_prompt 1
set --global __evanrelf_prompt_hostname (evanrelf-prompt hostname)
function fish_prompt
    set --local __evanrelf_prompt_pipestatus $pipestatus
    set --local __evanrelf_prompt_pwd (pwd)
    set --local __evanrelf_prompt_jobs (jobs -g 2>/dev/null | count)
    evanrelf-prompt prompt \
        --pipestatus "$__evanrelf_prompt_pipestatus" \
        --pwd "$__evanrelf_prompt_pwd" \
        --jobs "$__evanrelf_prompt_jobs" \
        --hostname "$__evanrelf_prompt_hostname" \
        $argv
end
        "#
        .trim()
    );

    Ok(())
}
