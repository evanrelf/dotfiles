final: prev:

let
  inherit (final.evan.lib) checkVersion;

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

  # ghciwatch =
  #   let version = "0.5.16"; in
  #   (checkVersion version prev.ghciwatch).overrideAttrs (attrs: rec {
  #     inherit version;
  #     src = final.fetchFromGitHub {
  #       owner = "MercuryTechnologies";
  #       repo = "ghciwatch";
  #       rev = "release/${version}";
  #       hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  #     };
  #     cargoDeps = attrs.cargoDeps.overrideAttrs (final.lib.const {
  #       name = "${attrs.pname}-${version}-vendor.tar.gz";
  #       inherit src;
  #       outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  #     });
  #   });

  gnugrep-gprefix =
    gprefix final.gnugrep;

  gnused-gprefix =
    gprefix final.gnused;

  # jujutsu =
  #   let version = "0.25.0"; in
  #   (checkVersion version prev.jujutsu).overrideAttrs (attrs: rec {
  #     inherit version;
  #     src = final.fetchFromGitHub {
  #       owner = "martinvonz";
  #       repo = "jj";
  #       rev = "v${version}";
  #       hash = "sha256-5J1ZfPNyniUK5D3Pt1aKuJ+/8vad3JPxCztBRY591N8=";
  #     };
  #     cargoDeps = attrs.cargoDeps.overrideAttrs (final.lib.const {
  #       name = "${attrs.pname}-${version}-vendor.tar.gz";
  #       inherit src;
  #       outputHash = "sha256-kuZ1zvb6H5QWjJSUYMq5tEywsQMC6187YJPUT1r4S5o=";
  #     });
  #   });

  kakoune-unwrapped =
    prev.kakoune-unwrapped.overrideAttrs (attrs: rec {
      version = final.inputs.kakoune.shortRev;
      src = final.inputs.kakoune;
      patches = [ ];
      preConfigure = ''
        ${attrs.preConfigure or ""}
        export version="${version}"
      '';
    });

  roc =
    final.inputs'.roc.packages.full.override (inputs: {
      compile-deps = (inputs.compile-deps or { }) // {
        zigPkg =
          assert inputs.compile-deps.zigPkg.version == "0.11.0";
          final.zigpkgs."0.11.0";
      };
    });
}
