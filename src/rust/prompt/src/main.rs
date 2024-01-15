use camino::Utf8PathBuf;
use clap::Parser as _;

#[derive(clap::Parser)]
struct Args {
    /// `$pipestatus`
    pipestatus: String,
    /// `(count (jobs --pid))`
    jobs: u8,
    /// `$COLUMNS`
    terminal_width: u16,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let prompt = prompt(args)?;
    println!("{prompt}");
    Ok(())
}

fn prompt(args: Args) -> anyhow::Result<String> {
    let status: Vec<u8> = todo!();
    let status: String = todo!();

    let hostname: String = todo!();

    let directory: Utf8PathBuf = todo!();

    let nix_shell: bool = todo!();

    let jobs: u8 = todo!();

    Ok(format!(
        "{}{}{}{}{}\n$ ",
        status, hostname, directory, nix_shell, jobs
    ))
}
