{ inputs, lib, pkgs }:

{
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

  onDemand = true;
}
