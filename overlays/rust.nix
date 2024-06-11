final: prev:

{
  home-rebuild =
    final.crane.buildPackage {
      src = ../src/rust/home-rebuild;
    };
}
