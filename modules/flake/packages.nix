{ inputs, ... }:

{
  perSystem = { pkgs, system, ... }:
    let
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

      crane =
        inputs.crane.mkLib pkgs;

      rustCrane = { name, src, cargoLock ? null }:
        let
          commonArgs = {
            pname = name;
            version = "0.0.0";
            src = crane.cleanCargoSource src;
            strictDeps = true;
          };
          cargoArtifacts = crane.buildDepsOnly commonArgs;
        in
        crane.buildPackage (commonArgs // { inherit cargoArtifacts; });

      rustNaersk = { name, src, cargoLock ? null }:
        pkgs.naersk.buildPackage { inherit name src; };

      rustNixpkgs = { name, src, cargoLock ? { } }:
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
          rust {
            name = "empath";
            src = inputs.empath.outPath;
          };

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
          rustCrane {
            name = "evanrelf-prompt";
            src = ../../src/evanrelf-prompt;
          };

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
          rust {
            name = "hsl";
            src = inputs.hsl.outPath;
          };

        indigo =
          inputs.indigo.packages.${system}.default;

        infer-indent =
          rust {
            name = "infer-indent";
            src = ../../src/infer-indent;
          };

        jujutsu =
          pkgs.callPackage "${inputs.nixpkgs-master}/pkgs/by-name/ju/jujutsu/package.nix" { };

        kakoune =
          let
            sources = {
              better-haskell-kak = inputs.better-haskell-kak.outPath;
              byline-kak = inputs.byline-kak.outPath;
              locus-kak = inputs.locus-kak.outPath;
              open-github-kak = inputs.open-github-kak.outPath;
            };
          in
          pkgs.wrapKakoune pkgs.kakoune-unwrapped {
            plugins =
              builtins.attrValues
                (builtins.mapAttrs
                  (name: src: pkgs.kakouneUtils.buildKakounePluginFrom2Nix {
                    inherit name src;
                  })
                  sources);
          };

        pancase =
          rust {
            name = "pancase";
            src = inputs.pancase.outPath;
          };

        shrink-conflicts =
          rust {
            name = "shrink-conflicts";
            src = pkgs.fetchFromGitHub {
              owner = "asayers";
              repo = "shrink-conflicts";
              rev = "34e4f28677bc1cdba181c39b3b6f3e829ec26102";
              hash = "sha256-V7vGiBq8jfc8eSFHwStoblnNN0fRLXL1UBlkQWst3Fo=";
            };
          };
      };
    };
}
