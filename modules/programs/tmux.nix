{ inputs, lib, pkgs, ... }:

{
  home.packages = [ pkgs.tmux ];

  xdg.configFile."tmux/tmux.conf".source =
    ../../configs/tmux/.config/tmux/tmux.conf;

  xdg.configFile."tmux/plugins/tpm".source = "${inputs.tpm}";
}
