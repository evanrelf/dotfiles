pkgsFinal: pkgsPrev:

{
  cached-nix-shell =
    let
      src =
        pkgsPrev.fetchFromGitHub {
          owner = "uri-canva";
          repo = "cached-nix-shell";
          rev = "cb1c60a6d8e8eefb20097c6c5937523d0a2dabb9";
          sha256 = "sha256-Un/vvQSk8c1qY7NGynvMCHgEikRhqeRQxVFPlV8Zabs=";
        };
    in
    import src { pkgs = pkgsFinal; };

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
