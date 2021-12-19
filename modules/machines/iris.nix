{ pkgs, ... }:

{
  imports = [
    ./common.nix
    ./personal.nix
  ];

  home.packages = with pkgs; [
    gnupg
    pinentry-curses
  ];

  xdg.configFile."fish/local.fish".text = ''
    set --export GPG_TTY (tty)
  '';

  home.file.".gnupg/gpg-agent.conf" = {
    text = ''
      pinentry-program /home/evan/.nix-profile/bin/pinentry-curses
    '';
    onChange = ''
      chmod 700 "$HOME"/.gnupg
    '';
  };
}
