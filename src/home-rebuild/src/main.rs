#![allow(clippy::collapsible_else_if)]

use anyhow::Context as _;
use std::process::{Command, Stdio};

fn main() -> Result<(), anyhow::Error> {
    let supports_flakes = {
        let output = Command::new("nix-instantiate")
            .args(["--eval", "--expr", "builtins ? getFlake", "--json"])
            .stdout(Stdio::piped())
            .spawn()
            .context("Failed to execute nix-instantiate")?
            .wait_with_output()
            .context("nix-instantiate wasn't running")?;

        match output.status.code() {
            Some(0) => String::from_utf8(output.stdout)
                .context("Failed to convert output from nix-instantiate into a UTF-8 string")?
                .trim_end()
                .parse()
                .context("Failed to parse output from nix-instantiate into a bool")?,
            Some(code) => anyhow::bail!("nix-instantiate failed with exit code {code}"),
            None => anyhow::bail!("nix-instantiate terminated by a signal"),
        }
    };

    let hostname = {
        let output = Command::new("hostname")
            .arg("-s")
            .output()
            .context("Failed to execute hostname")?;

        match output.status.code() {
            Some(0) => String::from_utf8(output.stdout)
                .context("Failed to convert output from hostname into a UTF-8 string")?,
            Some(code) => anyhow::bail!("hostname failed with exit code {code}"),
            None => anyhow::bail!("hostname terminated by a signal"),
        }
    };
    let hostname = hostname.trim_end();

    if supports_flakes {
        let args = std::env::args().skip(1).collect::<Vec<_>>();

        let exit_status = Command::new("home-manager")
            .args(["--flake", &format!(".#{hostname}")])
            .args(args)
            .spawn()
            .context("Failed to execute home-manager")?
            .wait()
            .context("home-manager wasn't running")?;

        match exit_status.code() {
            Some(0) => {}
            Some(code) => anyhow::bail!("home-manager failed with exit code {code}"),
            None => anyhow::bail!("home-manager terminated by a signal"),
        }
    } else {
        let mut args = std::env::args().skip(1);

        enum Subcommand {
            Build,
            Switch,
        }

        let subcommand = match (args.next().as_deref(), args.next()) {
            (Some("build"), None) => Subcommand::Build,
            (Some("switch"), None) => Subcommand::Switch,
            _ => anyhow::bail!("usage: home-rebuild (build | switch)"),
        };

        let output = Command::new("nix-build")
            .args([
                "--attr",
                &format!("homeConfigurations.{hostname}.activation-script"),
            ])
            .stdout(Stdio::piped())
            .spawn()
            .context("Failed to execute nix-build")?
            .wait_with_output()
            .context("nix-build wasn't running")?;

        let store_path = match output.status.code() {
            Some(0) => String::from_utf8(output.stdout)
                .context("Failed to convert output from nix-build into a UTF-8 string")?,
            Some(code) => anyhow::bail!("nix-build failed with exit code {code}"),
            None => anyhow::bail!("nix-build terminated by a signal"),
        };
        let store_path = store_path.trim_end();

        match subcommand {
            Subcommand::Build => println!("{store_path}"),
            Subcommand::Switch => {
                let exit_status = Command::new(&format!("{store_path}/activate"))
                    .spawn()
                    .context("Failed to execute activate")?
                    .wait()
                    .context("activate wasn't running")?;

                match exit_status.code() {
                    Some(0) => {}
                    Some(code) => anyhow::bail!("activate failed with exit code {code}"),
                    None => anyhow::bail!("activate terminated by a signal"),
                };
            }
        }
    }

    Ok(())
}
