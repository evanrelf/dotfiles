final: prev:

let
  assertUpgrade = prevDrv: finalDrv:
    let
      older = builtins.compareVersions (prevDrv.version or "0") finalDrv.version < 0;
      error = builtins.throw ''
        '${finalDrv.pname}' override is outdated

        Version from Nixpkgs:  ${prevDrv.version}
        Version from dotfiles: ${finalDrv.version}
      '';
    in
    assert older || error; finalDrv;

  gprefix = drv:
    final.runCommandLocal "gprefix-${drv.name}" { } ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

  go = { name, src ? final.inputs.${name}.outPath, vendorHash ? null }:
    final.buildGoModule (attrs: {
      inherit name src vendorHash;
    });

  rust = rustNaersk;

  rustNaersk = { name, src ? final.inputs.${name}.outPath, cargoLock ? null }:
    final.naersk.buildPackage { inherit name src; };

  rustNixpkgs = { name, src ? final.inputs.${name}.outPath, cargoLock ? { } }:
    final.rustPlatform.buildRustPackage (attrs: {
      inherit name src;
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

  evanrelf-prompt =
    rust { name = "evanrelf-prompt"; src = ../src/evanrelf-prompt; };

  findutils-gprefix =
    gprefix final.findutils;

  gawkInteractive-gprefix =
    gprefix final.gawkInteractive;

  gnugrep-gprefix =
    gprefix final.gnugrep;

  gnused-gprefix =
    gprefix final.gnused;

  go-hello =
    go { name = "go-hello"; src = ../src/go-hello; };

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
