{ lib, ... }:

{
  xdg.configFile."hammerspoon/init.lua".source =
    ../../configs/hammerspoon/.config/hammerspoon/init.lua;

  home.activation.hammerspoonConfigLocation =
    lib.hm.dag.entryAfter [ "writeBarrier" ] ''
      $DRY_RUN_CMD defaults write org.hammerspoon.Hammerspoon \
        MJConfigFile "$HOME/.config/hammerspoon/init.lua"
    '';
}
