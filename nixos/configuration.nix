# vim: foldenable foldmethod=marker
{ config, pkgs, ... }:

{
  # PACKAGES {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    acpi
    binutils
    clang
    diff-so-fancy
    docker
    exa
    fd
    fish
    fzf
    git
    mkpasswd
    mosh
    neofetch
    neovim
    nodejs
    powertop
    ripgrep
    stack
    stow
    tmux

    # Desktop
    xorg.setxkbmap
    xorg.xbacklight
    xorg.xset
    xsel
    feh

    # Apps
    chromium
    rofi
    spotify
    xst
  ];

  programs = {
    fish.enable = true;
  };


  # FONTS {{{1
  fonts.fonts = with pkgs; [
    iosevka-bin
  ];


  # BOOT {{{1
  boot = {
    loader = {
      systemd-boot.enable = true;
      systemd-boot.editor = false;
      efi.canTouchEfiVariables = true;
    };
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    kernelModules = [ "acpi_call" ];
    initrd.luks.devices = [
      {
        name = "root";
        device = "/dev/disk/by-uuid/5e2e8df5-c276-4e65-bf10-020516074377";
        preLVM = true;
        allowDiscards = true;
      }
    ];
  };


  # NETOWRKING {{{1
  networking = {
    hostName = "nixos";
    wireless.enable = true;
  };


  # SERVICES {{{1
  services = {
    openssh.enable = true;
    tlp.enable = true;
    xserver = {
      enable = true;
      displayManager.lightdm.enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      desktopManager.xterm.enable = false;
      libinput = {
        enable = true;
        naturalScrolling = true;
        tapping = false;
      };
      wacom.enable = true;
    };
    # compton = {
    #   enable = true;
    #   fade = true;
    #   fadeDelta = 3;
    #   # activeOpacity = "0.8";
    #   # inactiveOpacity = "0.8";
    #   # shadow = true;
    #   # shadowOffsets = [ (-10) (-10) ];
    # };
    redshift = {
      enable = true;
      latitude = "34.0522";
      longitude = "-118.2437";
    };
    unclutter.enable = true;
  };


  # USERS {{{1
  users = {
    users.evanrelf = {
      description = "Evan Relf";
      home = "/home/evanrelf";
      initialPassword = "banana";
      shell = pkgs.fish;
      extraGroups = [ "wheel" "networkmanager" ];
      isNormalUser = true;
    };
  };


  # MISCELLANEOUS {{{1
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.bluetooth.enable = false;
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  time.timeZone = "America/Los_Angeles";

  security.sudo.extraConfig = ''
    %wheel ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/setkeycodes
  '';

  imports = [ ./hardware-configuration.nix ];

  system.stateVersion = "18.09";


  # }}}1
}
