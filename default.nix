{ raw ? false }:

let
  lock =
    builtins.fromJSON (builtins.readFile ./flake.lock);

  flake-compat =
    builtins.fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
      sha256 = lock.nodes.flake-compat.locked.narHash;
    };

  flake =
    import flake-compat { src = ./.; };

in
if raw then
  flake.defaultNix
else
  flake.defaultNix.packages.${builtins.currentSystem}
