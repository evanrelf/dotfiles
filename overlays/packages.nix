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
      pname = "cached-nix-shell";
      version = "0.0.0";
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
      ] ++ pkgsPrev.lib.optionals pkgsPrev.stdenv.isDarwin [
        pkgsFinal.libiconv
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
    pkgsPrev.emacs.pkgs.withPackages (p: [ p.vterm ]);

  findutils-gprefix =
    gprefix pkgsFinal.findutils;

  gawkInteractive-gprefix =
    gprefix pkgsFinal.gawkInteractive;

  gnugrep-gprefix =
    gprefix pkgsFinal.gnugrep;

  gnused-gprefix =
    gprefix pkgsFinal.gnused;

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (prev: rec {
      version = "evanrelf";
      src = pkgsPrev.inputs.kakoune;
      preConfigure = ''
        ${prev.preConfigure}
        export version="${version}"
      '';
    });

  neovim =
    pkgsPrev.neovim.override {
      extraLuaPackages = p: [ p.fennel ];
    };

  prqlc =
    pkgsFinal.crane.buildPackage rec {
      pname = "prqlc";
      version = "0.9.2";
      src = pkgsPrev.fetchFromGitHub {
        owner = "PRQL";
        repo = "prql";
        rev = "c0d769a32e887973437a509129f6fae68c5d4740";
        sha256 = "sha256-5w+EODFeI5h4geGPSAZyssZgDrsFJyqzGoQRr8mHazA=";
      };
      buildInputs =
        pkgsPrev.lib.optionals pkgsPrev.stdenv.isDarwin [
          pkgsFinal.darwin.apple_sdk.frameworks.CoreServices
          pkgsFinal.libiconv
        ];
      cargoExtraArgs = "--package prqlc";
    };

  qsv =
    let
      crane =
        pkgsFinal.crane.overrideToolchain pkgsFinal.fenix.minimal.toolchain;
    in
    crane.buildPackage {
      src = pkgsPrev.fetchFromGitHub {
        owner = "jqnatividad";
        repo = "qsv";
        rev = "0.74.0";
        hash = "sha256-zMxvA/dc1MoLn7z7y/yWKBc+cYCHI0MO0tiLMNcBKeY=";
      };
      buildInputs = [
        pkgsFinal.python3
      ] ++ pkgsPrev.lib.optionals pkgsPrev.stdenv.isDarwin [
        pkgsFinal.darwin.apple_sdk.frameworks.Security
        pkgsFinal.libiconv
      ];
      cargoExtraArgs = "--features all_full";
    };
}
