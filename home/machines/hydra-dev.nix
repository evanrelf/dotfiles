{ pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  home.packages = with pkgs; [
    gcc
    gnupg
    pinentry.tty
  ];

  home.file.".bash_profile".text = ''
    # Use `fish` as my shell without needing to modify `/etc/shells`
    shopt -q login_shell && exec fish --login
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
}
