args:

let
  rev = "c27e54de99df793756a5314f8fd5dd3e49d31927";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "0289m555hpmml5g6idicg1bckphww22p87s8qca2k046zkz0ykx0";
  };

  reset = {
    config = {};
    overlays = [];
  };

in
  import nixpkgs (reset // args)
