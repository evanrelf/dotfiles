{ inputs, lib, pkgs }:

let
  package =
    let
      crane = inputs.crane.mkLib pkgs;
      commonArgs = {
        pname = "demo";
        version = "0.0.0";
        src = crane.cleanCargoSource ./.;
        strictDeps = true;
        meta.mainProgram = "demo";
      };
      cargoArtifacts = crane.buildDepsOnly commonArgs;
    in
    crane.buildPackage (commonArgs // { inherit cargoArtifacts; });

in
{
  command = lib.getExe package;

  onDemand = true;

  jobs.tick = {
    command = "${pkgs.bash}/bin/bash -c 'date > $STATE_DIRECTORY/tick'";
    startAt = "hourly";
  };
}
