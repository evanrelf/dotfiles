{ inputs, ... }:

{
  perSystem = { pkgs, ... }:
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
        pkgs.runCommandLocal "gprefix-${drv.name}" { } ''
          mkdir -p "$out/bin"
          for bin in ${drv}/bin/*; do
            ln -s "$bin" "$out/bin/g$(basename $bin)"
          done
          ln -s ${drv}/share "$out/share"
        '';

      go = { name, src ? inputs.${name}.outPath, vendorHash ? null }:
        pkgs.buildGoModule (attrs: {
          inherit name src vendorHash;
        });

      rust = rustNaersk;

      rustNaersk = { name, src ? inputs.${name}.outPath, cargoLock ? null }:
        pkgs.naersk.buildPackage { inherit name src; };

      rustNixpkgs = { name, src ? inputs.${name}.outPath, cargoLock ? { } }:
        pkgs.rustPlatform.buildRustPackage (attrs: {
          inherit name src;
          cargoLock = (attrs.cargoLock or { }) // {
            lockFile = "${attrs.src}/Cargo.lock";
          } // cargoLock;
        });

      # TODO: Add `zig` function to easily package Zig projects.
    in
    {
      packages = {
        coreutils-gprefix =
          (pkgs.coreutils.override {
            singleBinary = false;
            withPrefix = true;
          }).overrideAttrs (attrs: {
            doCheck = false;
          });

        empath =
          rust { name = "empath"; };

        evanrelf-fish =
          let
            fish-colored-man =
              pkgs.fishPlugins.buildFishPlugin {
                pname = "fish-colored-man";
                version = "0-unstable-2021-07-15";
                src = pkgs.fetchFromGitHub {
                  owner = "decors";
                  repo = "fish-colored-man";
                  rev = "1ad8fff696d48c8bf173aa98f9dff39d7916de0e";
                  hash = "sha256-uoZ4eSFbZlsRfISIkJQp24qPUNqxeD0JbRb/gVdRYlA=";
                };
              };
          in
          pkgs.wrapFish {
            pluginPkgs = with pkgs.fishPlugins; [
              fish-colored-man
              fzf
            ];
          };

        evanrelf-prompt =
          rust { name = "evanrelf-prompt"; src = ../../src/evanrelf-prompt; };

        findutils-gprefix =
          gprefix pkgs.findutils;

        gawkInteractive-gprefix =
          gprefix pkgs.gawkInteractive;

        gnugrep-gprefix =
          gprefix pkgs.gnugrep;

        gnused-gprefix =
          gprefix pkgs.gnused;

        go-hello =
          go { name = "go-hello"; src = ../../src/go-hello; };

        hsl =
          rust { name = "hsl"; };

        pancase =
          rust { name = "pancase"; };
      };
    };
}
