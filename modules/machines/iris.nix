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

    if test -z "$SSH_AUTH_SOCK" -a -n "$XDG_RUNTIME_DIR"
      set --export SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent"
    end
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
