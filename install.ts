#!/usr/bin/env -S deno run --unstable --allow-env --allow-read --allow-run

// TODO: Curse you async!

import * as _ from "https://deno.land/x/ramda/index.js";
import * as fs from "https://deno.land/std@0.63.0/fs/mod.ts";
import parseArgs from "https://deno.land/x/deno_minimist@v1.0.0/mod.ts";

const args: any = parseArgs(Deno.args);

const pkgs: string[] = _.map(_.replace("/", ""), args["_"]);

const dryRun: boolean = args["dry-run"] || false;

const runBool = async (command: string): Promise<boolean> => {
  const { success } = await Deno.run({ cmd: ["sh", "-c", command] }).status();
  return success;
};

const run = async (command: string): Promise<void> => {
  if (dryRun) {
    console.log(`dry-run> ${command}`);
  } else {
    if (await !runBool(command)) Deno.exit(1);
  }
};

const checkInstalled = async (executable: string): Promise<void> => {
  const exists = await runBool(`command -v ${executable} >/dev/null`);
  if (!exists) {
    console.error(`Missing executable: ${executable}`);
    Deno.exit(1);
  }
};

const discardNonexistent = (pkgs: string[]): string[] => {
  const [existent, nonexistent]: [string[], string[]] = _.partition(
    fs.exists,
    pkgs,
  );
  _.forEach(
    (p: string): void => console.error(`[${p}] Configuration doesn't exist`),
    nonexistent,
  );
  return existent;
};

const prepare = (pkg: string): void => {
  const home: string | undefined = Deno.env.get("HOME");

  if (home === undefined) {
    console.error("Cannot get HOME environment variable");
    Deno.exit(1);
  }

  switch (pkg) {
    case "doom":
    case "emacs": {
      console.log(`[${pkg}] Setting up truecolor support`);
      run('"$HOME"/dotfiles/emacs/.config/emacs/setup-truecolor');
      break;
    }
    case "hammerspoon": {
      console.log("[hammerspoon] Changing config file location");
      checkInstalled("defaults");
      run(_.join(" ", [
        "defaults write org.hammerspoon.Hammerspoon MJConfigFile",
        '"$HOME"/.config/hammerspoon/init.lua',
      ]));
      break;
    }
    case "kakoune": {
      if (!fs.exists(`${home}/.config/kak/plugins/plug.kak`)) {
        console.log("[kakoune] Installing plug.kak");
        checkInstalled("git");
        run(_.join(" ", [
          "git clone --depth=1 'https://github.com/andreyorst/plug.kak.git'",
          '"$HOME"/.config/kak/plugins/plug.kak',
        ]));
      }
      break;
    }
    case "neovim": {
      if (!fs.exists(`${home}/.local/share/nvim/site/autoload/plug.vim`)) {
        console.log("[neovim] Installing vim-plug");
        checkInstalled("curl");
        run(_.join(" ", [
          "curl --location --fail --create-dirs",
          "'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'",
          '-o "$HOME"/.local/share/nvim/site/autoload/plug.vim',
        ]));
      }
      break;
    }
    case "tmux": {
      if (!fs.exists(`${home}/.config/tmux/plugins/tpm`)) {
        console.log("[tmux] Installing tpm");
        checkInstalled("git");
        run(_.join(" ", [
          "git clone --depth=1 'https://github.com/tmux-plugins/tpm.git'",
          '"$HOME"/.config/tmux/plugins/tpm',
        ]));
      }
      break;
    }
  }
};

const stow = (pkg: string): void => {
  console.log(`[${pkg}] Stowing configuration`);
  checkInstalled("stow");
  run(`stow --stow --target="$HOME" --no-folding ${pkg}`);
};

const install = (pkg: string): void => {
  prepare(pkg);
  stow(pkg);
};

const main = (): void => {
  if (_.isEmpty(pkgs)) {
    console.error("No packages specified");
    Deno.exit(1);
  }

  _.forEach(install, discardNonexistent(pkgs));
};

main();
