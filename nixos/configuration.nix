{ config, pkgs, ... }:

{
  # IMPORTS
  imports = [
    <nixos-hardware/lenovo/thinkpad/x1/6th-gen>
    ./hardware-configuration.nix
  ];


  # PROGRAMS {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    binutils
  ];
  programs.fish.enable = true;
  programs.light.enable = true;
  programs.gnupg.agent.enable = true;
  services.openssh.enable = true;


  # FONTS {{{1
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [ iosevka-bin ];
  };


  # DESKTOP {{{1
  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    sensitivity = 200; # 0-255 (default 128)
    speed = 125; # 0-255 (default 97)
  };
  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [ swaylock swayidle xwayland ];
  };
  # services.xserver = {
  #   enable = true;
  #   dpi = 144;
  #   autoRepeatDelay = 200;
  #   autoRepeatInterval = 35;
  #   libinput = {
  #     enable = true;
  #     naturalScrolling = true;
  #     tappingDragLock = false;
  #     middleEmulation = false;
  #     accelSpeed = "0.7";
  #   };
  #   windowManager = {
  #     default = "xmonad";
  #     xmonad = {
  #       enable = true;
  #       enableContribAndExtras = true;
  #     };
  #   };
  #   desktopManager.xterm.enable = false;
  # };
  # services.xbanish.enable = true;
  services.physlock.enable = true;


  # SOUND {{{1
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
  };


  # POWER {{{1
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  services.tlp.enable = true;
  services.thermald.enable = true;
  services.undervolt = {
    enable = true;
    coreOffset = "-50";
    gpuOffset = "-50";
  };


  # SECURITY {{{1
  hardware.u2f.enable = true;


  # NETWORKING {{{1
  hardware.bluetooth.enable = false;
  networking.hostName = "sienna";
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };


  # SERVICES {{{1
  systemd.services = {
    "keyswap" = {
      description = "Key swap";
      enable = true;
      wantedBy = [ "multi-user.target" ];
      script = ''
        export PATH=/run/current-system/sw/bin:$PATH
        setkeycodes 3a 1
        setkeycodes 38 125
        setkeycodes e05b 56
      '';
    };
  };


  # USERS {{{1
  nix.trustedUsers = [ "root" "@wheel" ];
  users.users."evanrelf" = {
    description = "Evan Relf";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "audio"
      "video"
      "networkmanager"
    ];
    initialPassword = "banana";
    shell = pkgs.fish;
  };


  # BOOT {{{1
  boot = {
    tmpOnTmpfs = true;
    loader.efi.canTouchEfiVariables = true;
    loader.systemd-boot = {
      enable = true;
      editor = false;
    };
    initrd.luks.devices = {
      root = {
        device = "/dev/disk/by-uuid/424daca7-06a2-429a-85e1-1f0a17799bed";
        preLVM = true;
        allowDiscards = true;
      };
    };
    extraModulePackages = with pkgs.linuxPackages; [ acpi_call ];
    kernelModules = [ "acpi_call" ];
  };


  # MISCELLANEOUS {{{1
  hardware.cpu.intel.updateMicrocode = true;
  time.timeZone = "America/Los_Angeles";
  services.timesyncd.enable = true;
  services.printing.enable = true;
  system.stateVersion = "20.03";


  # }}}1
}

# vim: foldenable foldmethod=marker
