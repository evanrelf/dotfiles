# "azure" referring to my work machine, not Microsoft's cloud :-)

# TODO:
# - Automate setup for substitution and distributed builds (for host and guest
#   VM)

{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "azure-vm";
  networking.useDHCP = false;
  networking.interfaces.ens33.useDHCP = true;
  networking.firewall.allowedTCPPorts = [ 5000 ];

  services.openssh.enable = true;
  security.sudo.wheelNeedsPassword = false;

  virtualisation.vmware.guest = {
    enable = true;
    headless = true;
  };

  nix.trustedUsers = [ "root" "@wheel" ];

  services.nix-serve = {
    enable = true;
    secretKeyFile = "/var/cache-priv-key.pem";
  };

  users = {
    mutableUsers = false;
    users.root.initialPassword = "alpine";
    users.evan = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      initialPassword = "banana";
    };
  };

  time.timeZone = "America/Los_Angeles";

  system.stateVersion = "20.09";
}
