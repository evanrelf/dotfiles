{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.homebrew;

  brewfile =
    let
      prefixLinesWith = prefix: lines:
        builtins.concatStringsSep
          "\n"
          (builtins.map
            (line: "${prefix} \"${line}\"")
            lines);

    in
    pkgs.writeText "Brewfile" ''
      ${prefixLinesWith "brew" cfg.formulae}
      ${lib.optionalString (builtins.length cfg.casks > 0) "tap \"homebrew/cask\""}
      ${prefixLinesWith "cask" cfg.casks}
    '';

in
{
  options = {
    dotfiles.programs.homebrew = {
      enable = lib.mkEnableOption "homebrew";

      upgrade = lib.mkOption {
        description = "Run `brew upgrade` on dependencies";
        type = lib.types.bool;
        default = true;
      };

      cleanup = lib.mkOption {
        description = "Run `brew bundle cleanup` to uninstall dependencies from outside of Home Manager";
        type = lib.types.bool;
        default = false;
      };

      zap = lib.mkOption {
        description = "Use `--zap` flag when uninstalling dependencies with `brew bundle cleanup`";
        type = lib.types.bool;
        default = false;
      };

      formulae = lib.mkOption {
        description = "Formulae to install";
        type = lib.types.listOf lib.types.string;
        default = [ ];
      };

      casks = lib.mkOption {
        description = "Casks to install";
        type = lib.types.listOf lib.types.string;
        default = [ ];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.isDarwin;
        message = "homebrew: Only works on macOS";
      }
    ];

    home.activation.brewBundle =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        export HOMEBREW_BUNDLE_FILE="${brewfile}"
        $DRY_RUN_CMD brew bundle cleanup \
          ${lib.optionalString cfg.cleanup "--force"} \
          ${lib.optionalString (cfg.cleanup && cfg.zap) "--zap"} \
          #
        if [ -v DRY_RUN ] || ! $DRY_RUN_CMD brew bundle check; then
          $DRY_RUN_CMD brew bundle install \
            ${lib.optionalString (!cfg.upgrade) "--no-upgrade"} \
            --no-lock
        fi
      '';
  };
}
