final: prev:

{
  comma-update =
    final.crane.buildPackage {
      src = ../src/rust/comma-update;
      nativeBuildInputs = [
        final.darwin.apple_sdk.frameworks.Security
      ];
    };

  home-rebuild =
    final.crane.buildPackage {
      src = ../src/rust/home-rebuild;
    };
}
