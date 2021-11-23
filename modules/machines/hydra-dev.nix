{ pkgs, ... }:

{
  imports = [
    ./awake.nix
    ./common.nix
  ];

  home.packages = with pkgs; [
    gcc
    gnupg
    pinentry-curses
  ];

  home.file.".bash_profile".text = ''
    # Forcibly set 'fish' as my shell, even though I can't modify '/etc/shells'
    if [ "$(basename "$SHELL")" != "fish" ]; then
      sudo chsh "$(whoami)" --shell "$(command -v fish)"
      exec fish --login
    fi
  '';

  xdg.configFile."fish/local.fish".text = ''
    set --export GPG_TTY (tty)
  '';
}
