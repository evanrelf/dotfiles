{ lib, pkgs, ... }:

{
  home.packages = [ pkgs.tmux ];

  xdg.configFile."tmux/tmux.conf".source =
    ../../configs/tmux/.config/tmux/tmux.conf;

  home.activation.installTmuxPluginManager =
    lib.hm.dag.entryAfter [ "writeBarrier" ] ''
      if [ ! -d "$HOME/.config/tmux/plugins/tpm" ]; then
        echo "Installing tpm"
        $DRY_RUN_CMD git clone --depth=1 \
          "https://github.com/tmux-plugins/tpm.git" \
          "$HOME/.config/tmux/plugins/tpm"
      fi
    '';
}
