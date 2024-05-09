final: prev:

let
  inherit (final.evan) checkVersion;

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

  # jujutsu =
  #   let version = "0.17.0"; in
  #   (checkVersion version prev.jujutsu).overrideAttrs (attrs: rec {
  #     inherit version;
  #     src = final.fetchFromGitHub {
  #       owner = "martinvonz";
  #       repo = "jj";
  #       rev = "v${version}";
  #       hash = "";
  #     };
  #     cargoDeps = attrs.cargoDeps.overrideAttrs (final.lib.const {
  #       name = "${attrs.pname}-${version}-vendor.tar.gz";
  #       inherit src;
  #       outputHash = "";
  #     });
  #   });

  kakoune-unwrapped =
    prev.kakoune-unwrapped.overrideAttrs (attrs: rec {
      version = final.inputs.kakoune.shortRev;
      src = final.inputs.kakoune;
      patches = [ ];
      preConfigure = ''
        ${attrs.preConfigure}
        export version="${version}"
      '';
    });

  roc =
    final.inputs'.roc.packages.cli;
}
