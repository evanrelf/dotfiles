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
    final.rustPlatform.buildRustPackage (attrs: {
      name = "empath";
      src = final.fetchFromGitHub {
        owner = "evanrelf";
        repo = attrs.name;
        rev = final.inputs.empath.rev;
        hash = final.inputs.empath.narHash;
      };
      cargoLock.lockFile = "${attrs.src}/Cargo.lock";
    });

  findutils-gprefix =
    gprefix final.findutils;

  gawkInteractive-gprefix =
    gprefix final.gawkInteractive;

  gnugrep-gprefix =
    gprefix final.gnugrep;

  gnused-gprefix =
    gprefix final.gnused;

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
    final.rustPlatform.buildRustPackage (attrs: {
      name = "pancase";
      src = final.fetchFromGitHub {
        owner = "evanrelf";
        repo = attrs.name;
        rev = final.inputs.pancase.rev;
        hash = final.inputs.pancase.narHash;
      };
      cargoLock.lockFile = "${attrs.src}/Cargo.lock";
    });

  rstoc =
    final.rustPlatform.buildRustPackage (attrs: {
      name = "rstoc";
      src = final.fetchFromGitHub {
        owner = "evanrelf";
        repo = attrs.name;
        rev = final.inputs.rstoc.rev;
        hash = final.inputs.rstoc.narHash;
      };
      cargoLock.lockFile = "${attrs.src}/Cargo.lock";
    });
}
