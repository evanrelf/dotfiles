final: prev:

let
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
  coreutils-gprefix =
    (prev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    }).overrideAttrs (attrs: {
      doCheck = false;
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
