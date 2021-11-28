{ config, lib, ... }:

let
  cfg = config.dotfiles.programs.hammerspoon;

in
{
  options = {
    dotfiles.programs.hammerspoon = {
      enable = lib.mkEnableOption "hammerspoon";
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."hammerspoon/init.lua".source =
      ../../configs/hammerspoon/.config/hammerspoon/init.lua;

    home.activation.hammerspoonConfigLocation =
      lib.hm.dag.entryAfter [ "writeBarrier" ] ''
        $DRY_RUN_CMD defaults write org.hammerspoon.Hammerspoon \
          MJConfigFile "$HOME/.config/hammerspoon/init.lua"
      '';
  };
}
