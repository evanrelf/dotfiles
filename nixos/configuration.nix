{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # PROGRAMS {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [

    autojump
    binutils
    cabal-install
    chromium
    direnv
    exa
    fd
    fzf
    git
    gitAndTools.diff-so-fancy
    git-revise
    gnupg
    haskellPackages.ghcid
    hlint
    kakoune
    kitty
    lorri
    neovim
    nnn
    nodejs
    pandoc
    ripgrep
    shellcheck
    spotify
    stack
    stow
    tealdeer
    tectonic
    tmux
    xclip
    xorg.xrdb

  ];
  programs.fish.enable = true;
  programs.mosh.enable = true;
  services.openssh.enable = true;


  # FONTS {{{1
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      iosevka-bin
    ];
    fontconfig.ultimate = {
      enable = true;
      preset = "osx";
    };
  };


  # DESKTOP {{{1
  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    sensitivity = 100; # 0-255 (default 128)
    speed = 80; # 0-255 (default 97)
  };
  # services.pantheon.contractor.enable = true;
  services.xserver = {
    enable = true;
    dpi = 144;
    autoRepeatDelay = 200;
    autoRepeatInterval = 35;
    libinput = {
      enable = true;
      naturalScrolling = true;
      tappingDragLock = false;
      middleEmulation = false;
      accelSpeed = "0.7";
    };
    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    displayManager.lightdm = {
      enable = true;
      autoLogin = {
        enable = true;
        user = "evanrelf";
      };
    };
    desktopManager = {
      pantheon.enable = true;
      xterm.enable = false;
      # default = "elementary";
    };
  };
  services.xbanish.enable = true;


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


  # NETWORK {{{1
  hardware.bluetooth.enable = true;
  networking.hostName = "evanrelf-nixos";
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };


  # SERVICES {{{1
  systemd.services = {
    "keyswap" = {
      description = "Key swap";
      enable = true;
      script = ''
        export PATH=/run/current-system/sw/bin:$PATH
        setkeycodes 3a 1
        setkeycodes 38 125
        setkeycodes e05b 56
      '';
      wantedBy = [ "multi-user.target" ];
    };
  };


  # USERS {{{1
  users.users."evanrelf" = {
    description = "Evan Relf";
    isNormalUser = true;
    extraGroups = [ "audio" "networkmanager" "wheel" ];
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
    initrd.luks.devices = [
      {
        name = "root";
        device = "/dev/disk/by-uuid/424daca7-06a2-429a-85e1-1f0a17799bed";
        preLVM = true;
        allowDiscards = true;
      }
    ];
    extraModulePackages = with pkgs.linuxPackages; [ acpi_call ];
    kernelModules = [ "acpi_call" ];
  };


  # MISCELLANEOUS {{{1
  hardware.cpu.intel.updateMicrocode = true;
  time.timeZone = "America/Los_Angeles";
  services.timesyncd.enable = true;
  services.printing.enable = true;
  system.stateVersion = "19.09";


  # }}}1
}

# vim: foldenable foldmethod=marker
