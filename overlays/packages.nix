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
    final.rustPlatform.buildRustPackage rec {
      name = "empath";
      src = final.fetchFromGitHub {
        owner = "evanrelf";
        repo = name;
        rev = "ac5d0ef1de447c55a038254c55cb6fd76504263b";
        hash = "sha256-/kfjpnbf3SJAzA/ZPIjmSeYbpPnpBOzGS4eq0J4fSGE=";
      };
      cargoHash = "sha256-nUUs+51ugEtT3bZZQ7Xy/g4dxAJQeNiKtHy7JMrHAr4=";
    };

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
    final.rustPlatform.buildRustPackage rec {
      name = "pancase";
      src = final.fetchFromGitHub {
        owner = "evanrelf";
        repo = name;
        rev = "4ea8768fcb1ce335652a49bcb66cd137b410d200";
        hash = "sha256-TBpTpTcEYIavI2QQXWUAPKRXfv8fXZkeWVeKOFVEpdM=";
      };
      cargoHash = "sha256-k0IiMh75TzSq4GkOrfqU7b81s+JqHHQt1gVbAruc4vk=";
    };

  rstoc =
    final.rustPlatform.buildRustPackage rec {
      name = "rstoc";
      src = final.fetchFromGitHub {
        owner = "evanrelf";
        repo = name;
        rev = "c21dbb053977aaa0efbaf657be2638a47ff49331";
        hash = "sha256-3VTklzizavdg6ZOkTxjaMFP2lHLfZLncz7Oh1Y6T3xY=";
      };
      cargoHash = "sha256-nouZia7k8Q1zWBaN+55LE2YsWs96jo727rsCEUQdBok=";
    };
}
