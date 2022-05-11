pkgsFinal: pkgsPrev:

{
  comma-update =
    pkgsPrev.naersk.buildPackage {
      root = ../src;
      src = ../src/comma-update;
    };

  home-rebuild =
    pkgsPrev.writeShellScriptBin "home-rebuild" ''
      set -euo pipefail
      IFS=$'\n\t'

      PATH="${pkgsPrev.lib.makeBinPath (with pkgsFinal; [ home-manager ])}:$PATH"


      flakes=$(nix-instantiate --eval --expr 'builtins ? getFlake' --json)

      if [ "$flakes" = "true" ]; then
        home-manager --flake .#"$(hostname -s)" "$@"
      else
        if [ $# -ne 1 ]; then
          echo "usage: home-rebuild (build | switch)" >&2
          exit 1
        fi
        command=$1
        case "$command" in
          build)
            nix-build --attr homeConfigurations."$(hostname -s)".activation-script
            ;;
          switch)
            path=$(nix-build --attr homeConfigurations."$(hostname -s)".activation-script)
            "$path"/activate
            ;;
          *)
            echo "Unsupported command: $command" >&2
            exit 1
        esac
      fi
    '';
}
