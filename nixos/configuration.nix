{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # PROGRAMS {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [

    # Apps
    chromium
    kitty

    # Haskell
    cabal-install
    haskellPackages.ghcid
    hlint
    stack

    # Everything else
    autojump
    binutils
    exa
    fd
    fzf
    git
    gitAndTools.diff-so-fancy
    kakoune
    neovim
    nodejs
    ripgrep
    rustup
    shellcheck
    stow
    tealdeer
    tmux
    xclip
    xorg.xrdb

  ];
  programs.fish.enable = true;
  programs.mosh.enable = true;
  services.openssh.enable = true;


  # FONTS {{{1
  fonts = {
    fonts = with pkgs; [
      # Regular
      roboto
      dejavu_fonts

      # Monospaced
      iosevka-bin
      inconsolata

      # Emoji and icons
      noto-fonts-emoji
      material-icons
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
  services.gnome3.chrome-gnome-shell.enable = true;
  services.xserver = {
    enable = true;
    autoRepeatDelay = 200;
    autoRepeatInterval = 50;
    libinput = {
      enable = true;
      naturalScrolling = true;
      tappingDragLock = false;
      middleEmulation = false;
      accelSpeed = "0.7";
    };
    displayManager.lightdm.enable = true;
    desktopManager = {
      default = "none";
      xterm.enable = false;
      gnome3.enable = true;
    };
  };
  services.xbanish.enable = true;


  # SOUND {{{1
  sound.enable = true;
  hardware.pulseaudio.enable = true;


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
  system.stateVersion = "19.03";


  # }}}1
}

# vim: foldenable foldmethod=marker
