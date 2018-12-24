{ config, pkgs, ... }:

{
  # PROGRAMS {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    adapta-gtk-theme
    autojump
    borgbackup
    chromium
    clang
    cquery
    docker
    entr
    exa
    fd
    feh
    ffmpeg
    fish
    fzf
    fzf
    git
    gitAndTools.diff-so-fancy
    gnome3.nautilus
    gnupg
    jq
    light
    linuxPackages.wireguard
    lxappearance-gtk3
    mosh
    mpv
    mupdf
    neovim
    nodePackages.prettier
    nodejs
    npm
    pandoc
    papirus-icon-theme
    polybar
    ranger
    rclone
    ripgrep
    rofi
    rsync
    rustup
    shellcheck
    spotify
    stack
    stow
    tealdeer
    tmux
    transmission-gtk
    wireguard-tools
    xclip
    xorg.xrdb
    xorg.xset
    xst
    youtube-dl
    zathura
  ];
  programs = {
    fish.enable = true;
    light.enable = true;
    mosh.enable = true;
    npm.enable = true;
  };
  virtualisation.docker.enable = true;
  services.openssh.enable = true;


  # FONTS {{{1
  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      iosevka-bin
      material-icons
      roboto
    ];
    fontconfig.ultimate = {
      enable = true;
      preset = "osx";
    };
  };


  # DESKTOP {{{1
  services = {
    xserver = {
      enable = true;
      desktopManager.xterm.enable = false;
    };
    xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    compton = {
      enable = true;
      fade = true;
      fadeDelta = 3;
    };
    redshift = {
      enable = true;
      latitude = "34.0522";
      longitude = "-118.2437";
    };
    unclutter.enable = true;
  };


  # INPUT {{{1
  services.xserver.libinput = {
    enable = true;
    naturalScrolling = true;
    tappingDragLock = false;
    middleEmulation = false;
  };


  # SOUND {{{1
  sound.enable = true;
  hardware.pulseaudio.enable = true;


  # POWER {{{1
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };
  services.tlp.enable = true;


  # SECURITY {{{1
  security.sudo.wheelNeedsPassword = false;
  hardware.u2f.enable = true;
  services.physlock.enable = true;


  # NETWORK {{{1
  hardware.bluetooth.powerOnBoot = false;
  networking = {
    wireless = {
      enable = true;
    };
    networkmanager = {
      enable = true;
      wifi.powersave = true;
    };
    hostName = "nixos";
  };


  # BACKUP {{{1
  services.borgbackup = {
    "nixos" = {
      paths = "/home/evanrelf";
      repo = "TODO";
      startAt = "daily";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "TODO";
      };
      compression = "zstd,10";
    };
  };


  # USERS {{{1
  users.users = {
    "root".initialPassword = "banana";
    "evanrelf" = {
      description = "Evan Relf";
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "networkmanager"
        "docker"
      ];
      initialPassword = "banana";
      shell = pkgs.fish;
    };
  };


  # BOOT {{{1
  boot = {
    extraModulePackages = with pkgs; [ linuxPackages.wireguard ];
  };


  # HARDWARE {{{1
  imports = [ ./hardware-configuration.nix ];
  hardware.cpu.intel.updateMicrocode = true;


  # MISCELLANEOUS {{{1
  time.timeZone = "America/Los_Angeles";
  system.stateVersion = "18.09";


  # }}}1
}

# vim: foldenable foldmethod=marker
