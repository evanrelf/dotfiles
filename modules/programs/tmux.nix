{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.tmux;

in
{
  options = {
    dotfiles.programs.tmux = {
      enable = lib.mkEnableOption "tmux";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.tmux ];

    xdg.configFile."tmux/tmux.conf".source =
      ../../configs/tmux/.config/tmux/tmux.conf;

    xdg.configFile."tmux/plugins/tpm".source = "${inputs.tpm}";
  };
}
