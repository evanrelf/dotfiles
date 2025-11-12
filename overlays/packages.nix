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
    prev.codex.overrideAttrs (rec {
      version = "0.0.0";
      src = final.fetchFromGitHub {
        owner = "openai";
        repo = "codex";
        rev = "eb1c651c00955d31e4aafcda73a89c78c4c9e871";
        hash = "sha256-iedF58t94fE1TU1sauctNNsku5EYROdFdeKkNf8OIRc=";
      };
      cargoDeps = final.rustPlatform.fetchCargoVendor {
        src = "${src}/codex-rs";
        hash = "sha256-rru7cWIjt7WAUwiCxln42C4OVo8WVjp4SWZM1TaSxjI=";
      };
      # Don't look for version number in `codex --help`.
      doInstallCheck = false;
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
}
