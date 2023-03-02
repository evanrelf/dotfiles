{ config, lib, modulesPath, pkgs, ... }:

{
  # Hardware
  imports = [
    ./hardware-configuration.nix
  ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  # ZFS
  boot.zfs.devNodes = "/dev/disk/by-uuid/10237604199205279791";
  networking.hostId = "1e0dfc20";

  # Networking
  networking.hostName = "ultraviolet-vm";
  services.openssh.enable = true;

  # Users
  users.mutableUsers = false;
  users.users.evanrelf = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
    initialPassword = "banana";
  };

  environment.systemPackages = [
    pkgs.file
    pkgs.kakoune
    pkgs.neovim
  ];

  nix.settings.trusted-users = [ "root" "@wheel" ];
  nix.settings.max-jobs = 8;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  programs.fish.enable = true;
  programs.fish.useBabelfish = true;

  programs.ssh.startAgent = true;

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor = "tty";

  time.timeZone = "America/Los_Angeles";

  system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
