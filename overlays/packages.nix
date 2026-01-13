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

  rust = { name, src ? final.inputs.${name}.outPath, cargoLock ? { } }:
    final.rustPlatform.buildRustPackage (attrs: {
      inherit name;
      inherit src;
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

  hsl =
    rust { name = "hsl"; };

  magika-cli =
    final.stdenvNoCC.mkDerivation rec {
      pname = "magika-cli";
      version = "1.0.1";
      src = {
        aarch64-darwin = final.fetchurl {
          url = "https://github.com/google/magika/releases/download/cli/v${version}/magika-aarch64-apple-darwin.tar.xz";
          hash = "sha256-mAaHvzRTa4I5ie12d7B6L7TnhkxJs2k1kfjylPOhslM=";
        };
        aarch64-linux = final.fetchurl {
          url = "https://github.com/google/magika/releases/download/cli/v${version}/magika-aarch64-unknown-linux-gnu.tar.xz";
          sha256 = "sha256-L+cfuvcqFaNk/2dM8S7mN8QPnIzTfvj0Y4uc2GORg9g=";
        };
        x86_64-linux = final.fetchurl {
          url = "https://github.com/google/magika/releases/download/cli/v${version}/magika-x86_64-unknown-linux-gnu.tar.xz";
          sha256 = "sha256-af7nFv8NL7YcImloMzpgAMfRXJdIzC0l4jobB9PiifA=";
        };
      }.${final.stdenv.hostPlatform.system};
      nativeBuildInputs = final.lib.optionals final.stdenv.isLinux [
        final.autoPatchelfHook
      ];
      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        install -m755 magika $out/bin/magika
        runHook postInstall
      '';
    };

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
