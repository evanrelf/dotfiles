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

  go = { name, src ? final.inputs.${name}.outPath, vendorHash ? null }:
    final.buildGoModule (attrs: {
      inherit name src vendorHash;
    });

  rust = { name, src ? final.inputs.${name}.outPath, cargoLock ? { } }:
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

  sprite =
    let
      version = "0.0.1-rc31";
      baseUrl = "https://sprites-binaries.t3.storage.dev/client/v${version}";
      src = {
        "aarch64-darwin" = final.fetchurl {
          url = "${baseUrl}/sprite-darwin-arm64.tar.gz";
          hash = "sha256-ULgnFMqG+wPEhMomeNhqwhokAcsIgFlvw4qA18CxyYc=";
        };
        "aarch64-linux" = final.fetchurl {
          url = "${baseUrl}/sprite-linux-arm64.tar.gz";
          hash = "sha256-hRv0DfUsZQdCVOeXkbJ7tvnCypopeYD8z/+LkN+p0ic=";
        };
        "x86_64-darwin" = final.fetchurl {
          url = "${baseUrl}/sprite-darwin-amd64.tar.gz";
          hash = "sha256-UCNr5irx1x0xb+D9HjWgc46ESlq5Te8pbKsAcYujBYM=";
        };
        "x86_64-linux" = final.fetchurl {
          url = "${baseUrl}/sprite-linux-amd64.tar.gz";
          hash = "sha256-tzM19cI1P83l1ioKRvTqE2ThXiP62JZzFbjBWVUqtQw=";
        };
      }.${final.stdenv.hostPlatform.system};
    in
    final.stdenv.mkDerivation {
      pname = "sprite";
      inherit version src;
      dontUnpack = true;
      dontConfigure = true;
      dontBuild = true;
      installPhase = ''
        runHook preInstall
        mkdir -p "$out/bin"
        tar -xzf "$src" -C "$out/bin" sprite
        chmod +x "$out/bin/sprite"
        runHook postInstall
      '';
      meta = with final.lib; {
        description = "Sprite CLI";
        homepage = "https://sprites.dev";
        license = licenses.unfree;
        mainProgram = "sprite";
        platforms = [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ];
      };
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
