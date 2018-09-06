# vim: foldenable foldmethod=marker
{ config, pkgs, ... }:

{
  # PACKAGES {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    acpi
    binutils
    clang
    dropbox-cli
    exa
    feh
    git
    gnumake
    htop
    mkpasswd
    neofetch
    neovim
    pipes
    powertop
    ripgrep
    scrot
    silver-searcher
    stack
    stow

    # Desktop
    dmenu
    lightlocker
    xorg.xbacklight

    # Themes & icons
    arc-theme
    arc-icon-theme
    adapta-gtk-theme
    papirus-icon-theme

    # Apps
    chromium
    emacs
    # qutebrowser
    spotify
    termite
    vscode
    wpa_supplicant_gui
  ];

  programs = {
    vim.defaultEditor = true;
    fish.enable = true;
    mosh.enable = true;
  };


  # FONTS {{{1
  fonts.fonts = with pkgs; [
    iosevka-bin
    noto-fonts
    gohufont
    terminus_font
  ];


  # BOOT {{{1
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  boot.kernelModules = [ "acpi_call" ];
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/9f49b01f-65ea-4e76-b7e3-69527fe787d5";
      preLVM = true;
      allowDiscards = true;
    }
  ];


  # NETOWRKING {{{1
  networking = {
    hostName = "thinkpad";
    wireless = {
      enable = true;
      userControlled.enable = true;
      interfaces = [ "wlp4s0" ];
      networks = {
          # REDACTED
        };
      };
    };
  };


  # SERVICES {{{1
  services = {
    openssh.enable = true;
    tlp.enable = true;
    xserver = {
      enable = true;
      displayManager.lightdm = {
        enable = true;
        autoLogin = {
          enable = true;
          user = "evanrelf";
        };
      };
      desktopManager.xfce.enable = true;
      desktopManager.xterm.enable = false;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      libinput = {
        enable = true;
        tapping = false;
        naturalScrolling = true;
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
    unclutter.enable = true;
    redshift = {
      enable = true;
      latitude = "34.0522";
      longitude = "-118.2437";
    };
  };


  # USERS {{{1
  users = {
    # mutableUsers = false;
    # users.root.hashedPassword = "REDACTED";
    users.evanrelf = {
      description = "Evan Relf";
      home = "/home/evanrelf";
      shell = pkgs.fish;
      extraGroups = [ "wheel" "networkmanager" ];
      isNormalUser = true;
      uid = 1000;
      # hashedPassword = "REDACTED";
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

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?


  # }}}1
}
