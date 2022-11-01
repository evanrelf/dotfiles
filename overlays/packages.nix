pkgsFinal: pkgsPrev:

let
  gprefix = drv:
    pkgsPrev.runCommandLocal "gprefix-${drv.name}" { } ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

in
{
  cached-nix-shell =
    let
      blake3 = pkgsPrev.fetchFromGitHub {
        owner = "BLAKE3-team";
        repo = "BLAKE3";
        rev = "0db6fddc86c2d35a125ec8fff22bcc09f61d3c84";
        hash = "sha256-/cRXeGzoAv0Sdq+mtIXJJ1NsU/mXUvaAecPhBxoNZCs=";
      };
    in
    (pkgsFinal.crane.buildPackage {
      # Add macOS support: https://github.com/xzfc/cached-nix-shell/pull/25
      src = pkgsPrev.fetchFromGitHub {
        owner = "uri-canva";
        repo = "cached-nix-shell";
        rev = "cb1c60a6d8e8eefb20097c6c5937523d0a2dabb9";
        sha256 = "sha256-Un/vvQSk8c1qY7NGynvMCHgEikRhqeRQxVFPlV8Zabs=";
      };
      buildInputs = [
        pkgsFinal.nix
        pkgsFinal.openssl
        pkgsFinal.ronn
      ];
      CNS_GIT_COMMIT = "next";
      BLAKE3_CSRC = "${blake3}/c";
    }).overrideAttrs (prev: {
      postBuild = "make -f nix/Makefile post-build";
      postInstall = "make -f nix/Makefile post-install";
    });

  coreutils-gprefix =
    (pkgsPrev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    }).overrideAttrs (prev: {
      doCheck = false;
    });

  emacs =
    pkgsFinal.emacsNativeComp;

  findutils-gprefix =
    gprefix pkgsFinal.findutils;

  gnugrep-gprefix =
    gprefix pkgsFinal.gnugrep;

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (prev: rec {
      version = "2022.10.31";
      src = pkgsPrev.fetchFromGitHub {
        repo = "kakoune";
        owner = "mawww";
        rev = "v${version}";
        hash = "sha256-vmzGaGl0KSjseSD/s6DXxvMUTmAle+Iv/ZP9llaFnXk=";
      };
      preConfigure = ''
        export version="v${version}"
      '';
    });
}
