#!/usr/bin/env deno run

// TODO: Deno doesn't have a way of distinguishing between files and directories
// when checking if paths exist

import * as Flags from "https://deno.land/std@0.50.0/flags/mod.ts";
import * as Fs from "https://deno.land/std@0.50.0/fs/mod.ts";
import * as R from "https://deno.land/x/ramda@v0.27.0/index.js";

let dryRun: boolean = false;

const run = (command: string) => {
  if (dryRun) {
    console.log(`dry-run> ${command}`);
  } else {
    Deno.run({ cmd: R.split(" ", command) });
  }
};

const checkInstalled = (executable: string) => {
  // TODO: Deno doesn't have a way of checking for executables in your PATH
};

const discardNonexistent = (pkgs: Array<string>) => {
  const [existent, nonexistent]: Array<Array<string>> = R.partition(
    Fs.existsSync,
    pkgs,
  );
  R.map(
    (x: string) => console.log(`[${x}] Configuration doesn't exist`),
    nonexistent,
  );
  return existent;
};

const prepare = (pkg: string) => {
  const home: string | undefined = Deno.env.get("HOME");
  switch (pkg) {
    case "hammerspoon": {
      console.log("[hammerspoon] Changing config file location");
      run(R.join(" ", [
        "defaults write org.hammerspoon.Hammerspoon MJConfigFile",
        `"${home}/.config/hammerspoon/init.lua"`,
      ]));
      break;
    }
    case "kakoune": {
      if (!Fs.existsSync(`${home}/.config/kak/plugins/plug.kak`)) {
        console.log("[kakoune] Installing plug.kak");
        checkInstalled("git");
        run(R.join(" ", [
          "git clone --depth=1 https://github.com/andreyorst/plug.kak.git",
          `"${home}/.config/kak/plugins/plug.kak"`,
        ]));
      }
      break;
    }
    case "neovim": {
      if (!Fs.existsSync(`${home}/.local/share/nvim/site/autoload/plug.vim`)) {
        console.log("[neovim] Installing vim-plug");
        checkInstalled("curl");
        run(R.join(" ", [
          "curl --location --fail --create-dirs",
          "'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'",
          `--output="${home}/.local/share/nvim/site/autoload/plug.vim"`,
        ]));
      }
      break;
    }
    case "tmux": {
      if (!Fs.existsSync(`${home}/.config/tmux/plugins/tpm`)) {
        console.log("[tmux] Installing tpm");
        checkInstalled("git");
        run(R.join(" ", [
          "git clone --depth=1 'https://github.com/tmux-plugins/tpm.git'",
          `"${home}/.config/tmux/plugins/tpm"`,
        ]));
      }
      break;
    }
  }
};

const stow = (pkg: string) => {
  const home: string | undefined = Deno.env.get("HOME");
  console.log(`[${pkg}] Stowing configuration`);
  checkInstalled("stow");
  run(`stow --stow --target="${home}" --no-folding ${pkg}`);
};

const install = (pkg: string) => {
  prepare(pkg);
  stow(pkg);
};

const main = () => {
  const args: any = Flags.parse(Deno.args, { boolean: ["dry-run"] });
  const pkgs: Array<string> = args._;
  if (args["dry-run"]) dryRun = true;
  if (R.length(pkgs) === 0) {
    console.log("No packages specified");
    Deno.exit(1);
  }
  R.map(install, discardNonexistent(pkgs));
};

main();
