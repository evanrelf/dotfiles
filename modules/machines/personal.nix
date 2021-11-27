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
  '';
}
