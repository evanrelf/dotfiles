{ pkgs, ... }:

{
  imports = [
    ./awake.nix
    ./common.nix
  ];

  home.packages = with pkgs; [
    bpftrace
    gcc
    gnupg
    nix
    pinentry.tty
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

  home.file.".gnupg/gpg-agent.conf" = {
    text = ''
      pinentry-program /home/evan/.nix-profile/bin/pinentry
    '';
    onChange = ''
      chmod 700 "$HOME"/.gnupg
    '';
  };

  xdg.configFile."nix/nix.conf".text = ''
    extra-experimental-features = nix-command flakes
  '';
}
