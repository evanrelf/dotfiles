let
  lib = import ./lib.nix;

  revisions = {
    "nixos-20.03" = {
      # ↓ Don't change this revision often
      rev = "5adf2a6c11646898742b0c08f7e94101620ba707";
      sha256 = "0wf7pwma2qyfak39b242mcq8z7cdj65sds7hcjxchy0448shapzi";
    };
    "nixpkgs-unstable" = {
      rev = "c27e54de99df793756a5314f8fd5dd3e49d31927";
      sha256 = "0289m555hpmml5g6idicg1bckphww22p87s8qca2k046zkz0ykx0";
    };
    "master" = {
      rev = "a48adb351116f1a0ab81fe551a36436e447cbfff";
      sha256 = "15jhg21b5an94himdrc93h55zr943miw070mprn3wxhv9b5n864b";
    };
  };

  aliases = rec {
    stable = revisions."nixos-20.03";
    unstable = revisions."nixpkgs-unstable";
    master = revisions."master";
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
