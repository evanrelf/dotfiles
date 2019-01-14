{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # PROGRAMS {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [

    acpi
    adapta-gtk-theme
    autojump
    binutils
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
    git
    gitAndTools.diff-so-fancy
    gnome3.nautilus
    gnupg
    haskellPackages.ghcid
    hlint
    jq
    light
    lxappearance-gtk3
    mosh
    mpv
    mupdf
    neofetch
    neovim
    nodePackages.prettier
    nodejs
    pandoc
    papirus-icon-theme
    polybar
    powertop
    ranger
    rclone
    ripgrep
    rofi
    rsync
    rustup
    scrot
    shellcheck
    spotify
    stack
    stow
    tealdeer
    texlive.combined.scheme-basic
    tmux
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
      terminus_font
    ];
    fontconfig.ultimate = {
      enable = true;
      preset = "osx";
    };
  };


  # DESKTOP {{{1
  services.xserver = {
    enable = true;
    libinput = {
      enable = true;
      naturalScrolling = true;
      tappingDragLock = false;
      middleEmulation = false;
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager.lightdm.enable = true;
    desktopManager.xterm.enable = false;
    xautolock = {
      enable = true;
      time = 5;
      locker = "${pkgs.systemd}/bin/systemctl suspend";
    };
  };
  services.redshift = {
    enable = true;
    latitude = "34.0522";
    longitude = "-118.2437";
  };
  services.unclutter-xfixes.enable = true;


  # SOUND {{{1
  sound.enable = true;
  hardware.pulseaudio.enable = true;


  # POWER {{{1
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };
  services.tlp.enable = true;
  # services.undervolt = {};


  # SECURITY {{{1
  security.sudo.wheelNeedsPassword = false;
  hardware.u2f.enable = true;
  services.physlock.enable = true;


  # NETWORK {{{1
  hardware.bluetooth.powerOnBoot = false;
  networking = {
    hostName = "nixos";
    networkmanager = {
      enable = true;
      wifi.powersave = true;
    };
  };


  # BACKUP {{{1
  # services.borgbackup.jobs = {
  #   "nixos" = {
  #     paths = "/home/evanrelf";
  #     repo = "/home/evanrelf/borg";
  #     startAt = "daily";
  #     doInit = true; # disable if using SSH or FUSE mount
  #     encryption = {
  #       mode = "repokey-blake2";
  #       passCommand = "TODO";
  #     };
  #     compression = "zstd,10";
  #   };
  # };


  # SERVICES {{{1
  systemd.services = {
    "keyswap" = {
      description = "Key swap";
      enable = true;
      script = "
      export PATH=/run/current-system/sw/bin:$PATH
      setkeycodes 3a 1
      setkeycodes 38 125
      setkeycodes e05b 56
      ";
      wantedBy = [ "multi-user.target" ];
    };
  };


  # USERS {{{1
  users.users = {
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
    extraModulePackages = with pkgs.linuxPackages; [
      acpi_call
      wireguard
    ];
    kernelModules = [
      "acpi_call"
      "wireguard"
    ];
  };


  # MISCELLANEOUS {{{1
  hardware.cpu.intel.updateMicrocode = true;
  time.timeZone = "America/Los_Angeles";
  services.timesyncd.enable = true;
  services.printing.enable = true;
  system.stateVersion = "18.09";


  # }}}1
}

# vim: foldenable foldmethod=marker
