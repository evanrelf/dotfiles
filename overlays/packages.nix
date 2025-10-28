final: prev:

let
  checkVersion = version: drv:
    let
      older = builtins.compareVersions (drv.version or "0") version < 0;
      error = builtins.throw ''
        '${drv.pname}' override is outdated

        Version from Nixpkgs:  ${drv.version}
        Version from dotfiles: ${version}
      '';
    in
    assert older || error; drv;

  gprefix = drv:
    final.runCommandLocal "gprefix-${drv.name}" { } ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

  rust = name:
    final.rustPlatform.buildRustPackage (attrs: {
      inherit name;
      src = final.inputs.${name}.outPath;
      cargoLock.lockFile = "${attrs.src}/Cargo.lock";
    });

in
{
  codex =
    prev.codex.overrideAttrs (attrs: {
      patches = (attrs.patches or [ ]) ++ [
        # Add Ctrl-N and Ctrl-P as aliases for Up and Down
        (final.fetchpatch {
          url = "https://github.com/openai/codex/pull/1994.diff";
          hash = "sha256-D4RszYfXaVslZm3UeymPr+DKI8leEmnfW8lxf7XNmPY=";
          relative = "codex-rs";
        })
      ];
    });

  coreutils-gprefix =
    (prev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    }).overrideAttrs (attrs: {
      doCheck = false;
    });

  empath =
    rust "empath";

  findutils-gprefix =
    gprefix final.findutils;

  gawkInteractive-gprefix =
    gprefix final.gawkInteractive;

  gnugrep-gprefix =
    gprefix final.gnugrep;

  gnused-gprefix =
    gprefix final.gnused;

  hsl =
    rust "hsl";

  # https://github.com/NixOS/nixpkgs/issues/456113
  jujutsu =
    if final.stdenv.hostPlatform.isDarwin then
      prev.jujutsu.override { rustPlatform = prev.rustPlatform // { buildRustPackage = prev.rustPlatform.buildRustPackage.override { cargoNextestHook = null; }; }; }
    else
      prev.jujutsu;

  # kakoune-unwrapped =
  #   prev.kakoune-unwrapped.overrideAttrs (attrs: rec {
  #     version = final.inputs.kakoune.shortRev;
  #     src = final.inputs.kakoune;
  #     patches = [ ];
  #     preConfigure = ''
  #       ${attrs.preConfigure or ""}
  #       export version="${version}"
  #     '';
  #   });

  pancase =
    rust "pancase";

  rstoc =
    rust "rstoc";
}
