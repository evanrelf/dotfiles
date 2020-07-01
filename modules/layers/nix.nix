{ config, lib, pkgs, ... }:

let
  cfg = config.layers.nix;

  # declarative-channels =
  #   let
  #     farm =
  #       pkgs.linkFarm "declarative-channels-farm"
  #         (pkgs.lib.mapAttrsToList
  #           (name: value: { inherit name; path = value; })
  #           cfg.declarativeChannels);
  #   in
  #     pkgs.runCommandLocal "declarative-channels" {} ''
  #       mkdir -p $out
  #       ln -s ${farm} $out/channels
  #     '';

  # declarativeChannelsOptions = { name ? "", config, ... }:
  #   { options = {};
  #   };

in
  { options = {
      layers.nix = {
        enable = lib.mkEnableOption "nix layer";

        # declarativeChannels = lib.mkOption {
        #   type = types.attrsOf (types.submodule declarativeChannelsOptions);
        #   default = {};
        # };
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = [
        # declarative-channels
        pkgs.cachix
        pkgs.comma
        pkgs.lorri
        pkgs.nix
        pkgs.nix-diff
        pkgs.nix-prefetch-git
      ];
    };
  }
