{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ffmpeg
    rclone
    youtube-dl
  ];

  dotfiles.programs.jujutsu = {
    enable = true;
    extraConfig = ''
      [user]
      name = "Evan Relf"
      email = "evan@evanrelf.com"
      [diff]
      format = "git"
    '';
  };

  xdg.configFile."git/local".text =
    lib.mkIf config.dotfiles.programs.git.enable ''
      [user]
        email = "evan@evanrelf.com"
        signingkey = "D85956120D0F33A6"
    '';
}
