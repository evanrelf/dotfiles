{ config, lib, pkgs, ... }:

let
  cfg = config.layers.kitty;

in
  { options = {
      layers.kitty = {
        enable = lib.mkEnableOption "kitty layer";
      };
    };

    config = lib.mkIf cfg.enable {
      # TODO: Don't add `pkgs.kitty` to `home.packages`
      programs.kitty = {
        enable = true;

        settings = import ./settings.nix;

        keybindings = import ./keybindings.nix;
      };
    };
  }
