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
}
