pkgsFinal: pkgsPrev:

{
  comma-update =
    pkgsPrev.writeShellScriptBin "comma-update" ''
      set -euo pipefail
      IFS=$'\n\t'

      PATH="${pkgsPrev.lib.makeBinPath (with pkgsFinal; [ coreutils wget ])}:$PATH"

      (
        filename="index-x86_64-$(uname | tr A-Z a-z)"

        mkdir -p ~/.cache/nix-index

        cd ~/.cache/nix-index

        wget \
          --timestamping \
          "https://github.com/Mic92/nix-index-database/releases/latest/download/$filename"

        ln -f $filename files
      )
    '';

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
