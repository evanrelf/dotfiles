{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.yabai;

in
{
  options = {
    dotfiles.programs.yabai = {
      enable = lib.mkEnableOption "yabai";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.isDarwin;
        message = "yabai: Only works on macOS";
      }
    ];

    xdg.configFile."yabai/yabairc".source =
      ../../configs/yabai/.config/yabai/yabairc;
  };
}
