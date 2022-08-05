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
}
