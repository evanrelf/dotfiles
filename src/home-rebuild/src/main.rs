#![allow(clippy::collapsible_else_if)]

use anyhow::Context as _;
use std::process::{Command, ExitStatus, Stdio};

fn main() -> Result<(), anyhow::Error> {
    let supports_flakes = {
        let output = Command::new("nix-instantiate")
            .args(["--eval", "--expr", "builtins ? getFlake", "--json"])
            .stderr(Stdio::inherit())
            .output()
            .context("Failed to execute nix-instantiate")?;

        handle_status("nix-instantiate", output.status)?;

        String::from_utf8(output.stdout)
            .context("Failed to convert output from nix-instantiate into a UTF-8 string")?
            .trim_end()
            .parse()
            .context("Failed to parse output from nix-instantiate into a bool")?
    };

    let hostname = {
        let output = Command::new("hostname")
            .arg("-s")
            .output()
            .context("Failed to execute hostname")?;

        handle_status("hostname", output.status)?;

        String::from_utf8(output.stdout)
            .context("Failed to convert output from hostname into a UTF-8 string")?
    };
    let hostname = hostname.trim_end();

    if supports_flakes {
        let args = std::env::args().skip(1).collect::<Vec<_>>();

        let exit_status = Command::new("home-manager")
            .args(["--flake", &format!(".#{hostname}")])
            .args(args)
            .status()
            .context("Failed to execute home-manager")?;

        handle_status("home-manager", exit_status)?;
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
            .stderr(Stdio::inherit())
            .output()
            .context("Failed to execute nix-build")?;

        handle_status("nix-build", output.status)?;

        let store_path = String::from_utf8(output.stdout)
            .context("Failed to convert output from nix-build into a UTF-8 string")?;
        let store_path = store_path.trim_end();

        match subcommand {
            Subcommand::Build => println!("{store_path}"),
            Subcommand::Switch => {
                let exit_status = Command::new(&format!("{store_path}/activate"))
                    .status()
                    .context("Failed to execute activate")?;

                handle_status("activate", exit_status)?;
            }
        }
    }

    Ok(())
}

fn handle_status(program: &str, status: ExitStatus) -> Result<(), anyhow::Error> {
    match status.code() {
        Some(0) => Ok(()),
        Some(code) => anyhow::bail!("{program} failed with exit code {code}"),
        None => anyhow::bail!("{program} terminated by a signal"),
    }
}
