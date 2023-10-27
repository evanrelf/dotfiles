#![allow(clippy::collapsible_else_if)]

use anyhow::Context as _;
use itertools::Itertools as _;
use xshell::{cmd, Shell};

fn main() -> anyhow::Result<()> {
    let sh = Shell::new()?;

    if supports_flakes(&sh)? {
        with_flakes(&sh)?;
    } else {
        without_flakes(&sh)?;
    }

    Ok(())
}

fn supports_flakes(sh: &Shell) -> anyhow::Result<bool> {
    cmd!(
        sh,
        "nix-instantiate --eval --expr 'builtins ? getFlake' --json"
    )
    .quiet()
    .read()
    .context("Failed to run nix-instantiate")?
    .trim_end()
    .parse()
    .context("Failed to parse output from nix-instantiate into a bool")
}

fn with_flakes(sh: &Shell) -> anyhow::Result<()> {
    let hostname = cmd!(sh, "hostname -s")
        .quiet()
        .read()
        .context("Failed to get hostname")?;

    let args = std::env::args().skip(1).join(" ");

    cmd!(sh, "home-manager --flake .#{hostname} {args}")
        .quiet()
        .run()
        .context("Failed to execute home-manager")?;

    Ok(())
}

fn without_flakes(sh: &Shell) -> anyhow::Result<()> {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    enum Subcommand {
        Build,
        Switch,
    }

    let subcommand = match (args.get(0).map(String::as_str), args.get(1..)) {
        (Some("build"), None) => Subcommand::Build,
        (Some("switch"), None) => Subcommand::Switch,
        _ => anyhow::bail!("usage: home-rebuild (build | switch)"),
    };

    let hostname = cmd!(sh, "hostname -s")
        .quiet()
        .read()
        .context("Failed to get hostname")?;

    let no_out_link = match subcommand {
        Subcommand::Build => "",
        Subcommand::Switch => "--no-out-link",
    };

    let store_path = cmd!(
        sh,
        "nix-build --attr homeConfigurations.{hostname}.activationPackage {no_out_link}"
    )
    .quiet()
    .read()
    .context("Failed to run nix-build")?;
    let store_path = store_path.trim_end();

    match subcommand {
        Subcommand::Build => println!("{store_path}"),
        Subcommand::Switch => {
            cmd!(sh, "{store_path}/activate")
                .quiet()
                .run()
                .context("Failed to run activate")?;
        }
    }

    Ok(())
}
