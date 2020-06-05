let
  lib = import ./lib.nix;

  revisions = {
    "nixos-19.09" = {
      # ↓ Don't change this revision
      rev = "856dbd1a5c7fd826cf3668ff12a7389be0686f41";
      sha256 = "1d895i1lc25d2akniaqg2n1jrg2rcd1gih8rpmhyrlv4lpggfmsx";
    };
    "nixos-20.03" = {
      rev = "5adf2a6c11646898742b0c08f7e94101620ba707";
      sha256 = "0wf7pwma2qyfak39b242mcq8z7cdj65sds7hcjxchy0448shapzi";
    };
    "nixpkgs-unstable" = {
      rev = "c27e54de99df793756a5314f8fd5dd3e49d31927";
      sha256 = "0289m555hpmml5g6idicg1bckphww22p87s8qca2k046zkz0ykx0";
    };
  };

  aliases = rec {
    legacy = revisions."nixos-19.09";
    stable = revisions."nixos-20.03";
    unstable = revisions."nixpkgs-unstable";
    default = unstable;
  };

  sources =
    let
      fetch = { rev, sha256 }:
        lib.fetchGitHub {
          owner = "nixos";
          repo = "nixpkgs";
          inherit rev sha256;
        };
    in
      builtins.mapAttrs (_: fetch) (revisions // aliases);

  derivations =
    builtins.mapAttrs
      (_: source: import source { config = {}; })
      sources;
in
  derivations // { inherit sources; }
