{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  home.username = "evanrelf";

  home.homeDirectory = "/Users/${config.home.username}";

  home.packages = [
    pkgs.aws-sso-creds
    pkgs.awscli2
  ];
}
