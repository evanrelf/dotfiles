let
  pkgs = import <nixpkgs> { };

  oxide-rfd-scraper =
    pkgs.haskellPackages.callCabal2nix "oxide-rfd-scraper" ./. { };

in
pkgs.mkShell {
  inputsFrom = [ oxide-rfd-scraper.env ];
  packages = with pkgs; [ cabal-install ];
}
