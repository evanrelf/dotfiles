let
  pkgs = import ../nix/.config/nix/pkgs.nix { };

  vmware = { modulesPath, ... }: {
    imports = [
      "${toString modulesPath}/virtualisation/vmware-image.nix"
    ];

    virtualisation.vmware.guest = {
      enable = true;
      headless = true;
    };
  };

  configuration = { config, lib, pkgs, ... }: {
    services.openssh.enable = true;
    security.sudo.wheelNeedsPassword = false;

    nix.trustedUsers = [ "root" "@wheel" ];
    nixpkgs.config.allowUnfree = true;

    programs.fish.enable = true;
    programs.gnupg.agent.enable = true;

    users = {
      mutableUsers = false;
      users.evanrelf = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
        shell = pkgs.fish;
        initialPassword = "banana";
      };
    };
  };

  nixos =
    import "${pkgs.path}/nixos/lib/eval-config.nix" {
      system = "x86_64-linux";
      modules = [
        vmware
        configuration
      ];
    };

in
nixos.config.system.build.vmwareImage
