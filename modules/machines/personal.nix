{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ffmpeg
    rclone
    youtube-dl
  ];

  xdg.configFile."git/local".text = ''
    [user]
      name = "Evan Relf"
      email = "evan@evanrelf.com"
  '';
}
