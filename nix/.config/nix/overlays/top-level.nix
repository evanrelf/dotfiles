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
  coreutils-gprefix =
    pkgsPrev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    };

  findutils-gprefix = gprefix pkgsFinal.findutils;

  gnugrep-gprefix = gprefix pkgsFinal.gnugrep;

  iosevka-bin = pkgsPrev.iosevka-bin.override { variant = "ss08"; };

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "545db22ae464668d36fad500690a77c019936910";
        sha256 = "08v31b70v815sjfzzz9jl3flk1k0ynv0n4dv68x0rkkkmxvzmnls";
      };
      preConfigure = ''
        ${old.preConfigure}
        export version="${version}"
      '';
    });

  neovim-unwrapped =
    pkgsPrev.neovim-unwrapped.overrideAttrs (old: {
      version = "0.5.0-dev+1157-g0ab88c2ea";
      src =
        pkgsPrev.fetchFromGitHub {
          owner = "neovim";
          repo = "neovim";
          rev = "0ab88c2ea80caa7cda97b3a8479d0d32e4636ab6";
          sha256 = "07jiv9c3032lrhmd3dvqv2v5l35bdn39jqi48qsjj220slrsrl53";
        };
      buildInputs = (old.buildInputs or [ ]) ++ [ pkgsFinal.tree-sitter ];
    });
}
