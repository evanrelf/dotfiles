{ config, lib, ... }:

{
  xdg.configFile."git/local".text =
    lib.mkIf config.dotfiles.programs.git.enable ''
      [user]
        email = "evan@awakesecurity.com"
        signingkey = "A9434F37F71F5BCA"
    '';
}
