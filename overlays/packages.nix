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

  rust = { name, cargoLock ? { } }:
    final.rustPlatform.buildRustPackage (attrs: {
      inherit name;
      src = final.inputs.${name}.outPath;
      cargoLock = (attrs.cargoLock or { }) // {
        lockFile = "${attrs.src}/Cargo.lock";
      } // cargoLock;
    });

in
{
  coreutils-gprefix =
    (prev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    }).overrideAttrs (attrs: {
      doCheck = false;
    });

  empath =
    rust { name = "empath"; };

  findutils-gprefix =
    gprefix final.findutils;

  gawkInteractive-gprefix =
    gprefix final.gawkInteractive;

  gnugrep-gprefix =
    gprefix final.gnugrep;

  gnused-gprefix =
    gprefix final.gnused;

  hsl =
    rust { name = "hsl"; };

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
    rust { name = "pancase"; };
}
