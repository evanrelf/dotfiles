{ pkgs, ... }:

{
  home.packages = with pkgs; [
    deno
    ffmpeg
    yt-dlp
    zig
  ];

  dotfiles.programs.jujutsu = {
    enable = false;
    config = ''
      [user]
      name = "Evan Relf"
      email = "evan@evanrelf.com"
      [diff]
      format = "git"
    '';
  };

  dotfiles.programs.git = {
    email = "evan@evanrelf.com";
    signingkey = "D85956120D0F33A6";
  };
}
