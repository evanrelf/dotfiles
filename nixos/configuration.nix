{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # PROGRAMS {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [

    acpi
    adapta-gtk-theme
    arandr
    autojump
    binutils
    chromium
    clang
    cmus
    cquery
    dunst
    entr
    exa
    fd
    feh
    ffmpeg
    fzf
    git
    gitAndTools.diff-so-fancy
    gnome3.nautilus
    gnumake
    gnupg
    haskellPackages.ghcid
    haskellPackages.xmobar
    hlint
    htop
    jq
    lxappearance
    mpc_cli
    mpd
    mpv
    mupdf
    ncmpcpp
    neofetch
    neovim
    nnn
    nodePackages.prettier
    nodejs
    notify-desktop
    pandoc
    papirus-icon-theme
    powertop
    ranger
    ripgrep
    rofi
    scrot
    shellcheck
    slack
    spotify
    stack
    stow
    tealdeer
    texlive.combined.scheme-basic
    tmux
    xclip
    xorg.xev
    xorg.xrdb
    xorg.xset
    xst
    youtube-dl
    zathura

    autokey
    firefox
    gimp
    rclone
    rsync
    rustup
    sxiv
    transmission-gtk

  ];
  programs = {
    fish.enable = true;
    light.enable = true;
    mosh.enable = true;
    npm.enable = true;
    chromium = {
      enable = true;
      extensions = [
        "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
        "kbmfpngjjgdllneeigpgjifpgocmfgmb" # Reddit Enhancement Suite
        "dneaehbmnbhcippjikoajpoabadpodje" # Old Reddit Redirect
        "dfoegpibjpjpchgmjnmomelfnclbijnm" # BazQux Open in Background Tab
        "eimadpbcbfnmbkopoojfekhnkhdbieeh" # Dark Reader
        "edemalkcamdhnnpkbdbokkflbcnhfhbi" # Mojave Pure Dark theme
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
        "pgdnlhfefecpicbbihgmbmffkjpaplco" # uBlock Origin Extra
        "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # Privacy Badger
        "ldpochfccmkkmhdbclfhpagapcfdljkj" # Decentraleyes
        "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
      ];
      extraOpts = {
        "DefaultSearchProviderEnabled" = "true";
        "DefaultSearchProviderSearchURL" = "https://www.startpage.com/do/dsearch?query={searchTerms}";
        "BlockThirdPartyCookies" = "true";
        "BrowserSignin" = "0";
        "SyncDisabled" = "true";
        "PasswordManagerEnabled" = "false";
        # "NewTabPageLocation" = "about:blank";
      };
    };
  };
  virtualisation.docker.enable = true;
  services.openssh.enable = true;


  # FONTS {{{1
  fonts = {
    fonts = with pkgs; [
      # Regular
      dejavu_fonts
      liberation_ttf
      libertinus

      # Monospaced
      iosevka-bin
      inconsolata
      anonymousPro

      # Bitmap
      terminus_font
      gohufont

      # Emoji and icons
      noto-fonts-emoji
      material-icons
      emacs-all-the-icons-fonts
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
      accelSpeed = "0.3";
    };
    wacom.enable = true;
    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      bspwm.enable = true;
      awesome.enable = true;
    };
    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = "evanrelf";
    };
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    xautolock = {
      enable = true;
      time = 5;
      enableNotifier = true;
      notify = 10;
      notifier = "${pkgs.notify-desktop} -t 10000 'Sleeping in 10s...'";
      locker = "${pkgs.systemd}/bin/systemctl suspend";
    };
  };
  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "opengl-swc";
  };
  services.redshift = {
    enable = true;
    latitude = "34.0522";
    longitude = "-118.2437";
  };
  services.xbanish.enable = true;


  # SOUND {{{1
  sound.enable = true;
  hardware.pulseaudio.enable = true;


  # POWER {{{1
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  services.tlp.enable = true;
  # services.undervolt = {};


  # SECURITY {{{1
  security.sudo.wheelNeedsPassword = false;
  hardware.u2f.enable = true;
  services.physlock = {
    enable = true;
    lockOn.hibernate = false;
  };


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
  users.users."evanrelf" = {
    description = "Evan Relf";
    isNormalUser = true;
    extraGroups = [
      "audio"
      "docker"
      "networkmanager"
      "wheel"
    ];
    initialPassword = "banana";
    shell = pkgs.fish;
  };


  # BOOT {{{1
  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.systemd-boot = {
      enable = true;
      editor = false;
      consoleMode = "auto";
    };
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
