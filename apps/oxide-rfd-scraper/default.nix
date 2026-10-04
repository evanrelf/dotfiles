{ inputs, lib, pkgs }:

let
  oxide-rfd-scraper =
    pkgs.haskellPackages.callCabal2nix "oxide-rfd-scraper" ./. { };

in
{
  command = "${oxide-rfd-scraper}/bin/oxide-rfd-scraper syndicate";

  onDemand = true;

  jobs.scrape = {
    command = "${oxide-rfd-scraper}/bin/oxide-rfd-scraper scrape";
    startAt = "daily";
  };
}
