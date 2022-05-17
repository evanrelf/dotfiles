use anyhow::{anyhow, Context};
use std::{env, fs, io::Read, path::Path};

fn main() -> Result<(), anyhow::Error> {
    let home = env::var("HOME").context("Failed to get $HOME")?;

    let cache_dir = format!("{home}/.cache/nix-index");

    fs::create_dir_all(&cache_dir).context("Failed to create cache directory")?;

    env::set_current_dir(&cache_dir).context("Failed to move into cache directory")?;

    let arch = match env::consts::ARCH {
        "x86_64" => "x86_64",
        unsupported => {
            eprintln!("warning: Unsupported arch {unsupported}, falling back to x86_64");
            "x86_64"
        }
    };

    let os = match env::consts::OS {
        "linux" => "linux",
        "macos" => "darwin",
        unsupported => anyhow::bail!("Unsupported OS: {unsupported}"),
    };

    let filename = format!("index-{arch}-{os}");

    println!("Downloading index");
    let response = ureq::get(&format!(
        "https://github.com/Mic92/nix-index-database/releases/latest/download/{filename}"
    ))
    .call()
    .context("Failed to send download request to GitHub")?;

    let response_length = response
        .header("Content-Length")
        .ok_or_else(|| anyhow!("Failed to get 'Content-Length' of response"))?
        .parse()
        .context("Failed to parse 'Content-Length' into a number")?;

    let mut response_bytes = Vec::with_capacity(response_length);
    response
        .into_reader()
        .read_to_end(&mut response_bytes)
        .context("Failed to read response")?;

    fs::write(&filename, response_bytes).context("Failed to write index to disk")?;

    println!("Download complete");

    let _ = fs::remove_file("files");

    if Path::new("files").exists() {
        anyhow::bail!("Failed to remove old index link");
    }

    fs::hard_link(&filename, "files").context("Faild to create hard link")?;

    Ok(())
}
