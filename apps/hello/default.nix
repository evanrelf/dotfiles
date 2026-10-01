{ inputs, lib, pkgs }:

{
  package =
    let
      crane = inputs.crane.mkLib pkgs;
      commonArgs = {
        pname = "hello";
        version = "0.0.0";
        src = crane.cleanCargoSource ./.;
        strictDeps = true;
        meta.mainProgram = "hello";
      };
      cargoArtifacts = crane.buildDepsOnly commonArgs;
    in
    crane.buildPackage (commonArgs // { inherit cargoArtifacts; });

  port = 3000;
}
