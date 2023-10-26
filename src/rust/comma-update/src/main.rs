use anyhow::Context as _;
use std::{env, fs, io::Read as _};

fn main() -> anyhow::Result<()> {
    let home = env::var("HOME").context("Failed to get $HOME")?;

    let cache_dir = format!("{home}/.cache/nix-index");

    fs::create_dir_all(&cache_dir).context("Failed to create cache directory")?;

    env::set_current_dir(&cache_dir).context("Failed to move into cache directory")?;

    let arch = match env::consts::ARCH {
        "x86_64" => "x86_64",
        "aarch64" => "aarch64",
        unsupported => anyhow::bail!("Unsupported arch: {unsupported}"),
    };

    let os = match env::consts::OS {
        "linux" => "linux",
        "macos" => "darwin",
        unsupported => anyhow::bail!("Unsupported OS: {unsupported}"),
    };

    let filename = format!("index-{arch}-{os}");

    let response = ureq::get(&format!(
        "https://github.com/Mic92/nix-index-database/releases/latest/download/{filename}"
    ))
    .call()
    .context("Failed to download from GitHub")?;

    let response_length = response
        .header("Content-Length")
        .context("Response lacks a 'Content-Length' header")?
        .parse()
        .context("Failed to parse 'Content-Length' into a number")?;

    let mut response_bytes = Vec::with_capacity(response_length);
    response
        .into_reader()
        .read_to_end(&mut response_bytes)
        .context("Failed to read response")?;

    fs::write(&filename, response_bytes).context("Failed to write index to disk")?;

    fs::remove_file("files").context("Failed to remove old index link")?;

    fs::hard_link(&filename, "files").context("Failed to create hard link")?;

    Ok(())
}
