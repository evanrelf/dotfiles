{ config, lib, pkgs, ... }:

let
  cfg = config.layers.fun;

in
  { options = {
      layers.fun = {
        enable = lib.mkEnableOption "fun layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        aalib
        asciiquarium
        cmatrix
        cowsay
        fortune
        gay
        gti
        lolcat
        pipes
        sl
        toilet
      ];
    };
  }
