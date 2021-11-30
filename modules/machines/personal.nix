{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ffmpeg
    rclone
    youtube-dl
  ];

  xdg.configFile."git/local".text = ''
    [user]
      email = "evan@evanrelf.com"
      signingkey = "D85956120D0F33A6"
  '';
}
