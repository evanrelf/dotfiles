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
  #   let version = "0.11.0"; in
  #   (checkVersion version prev.jujutsu).overrideAttrs (attrs: rec {
  #     inherit version;
  #     src = final.fetchFromGitHub {
  #       owner = "martinvonz";
  #       repo = "jj";
  #       rev = "v${version}";
  #       hash = "sha256-yEW7+0MnJlW0WeZ6UItaCDrihPLA52mLcu15tJwZx9w=";
  #     };
  #     cargoDeps = attrs.cargoDeps.overrideAttrs (final.lib.const {
  #       name = "${attrs.pname}-${version}-vendor.tar.gz";
  #       inherit src;
  #       outputHash = "sha256-xA9SDq1Kc0u8qFEPFFCic9uwE2Y/BXJzUHBCs1Czxtw=";
  #     });
  #   });

  # kakoune-unwrapped =
  #   prev.kakoune-unwrapped.overrideAttrs (attrs: rec {
  #     version = "HEAD";
  #     src = final.inputs.kakoune;
  #     preConfigure = ''
  #       ${attrs.preConfigure}
  #       export version="${version}"
  #     '';
  #   });
}
