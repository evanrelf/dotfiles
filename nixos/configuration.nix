{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # PROGRAMS {{{1
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [

    acpi
    adapta-gtk-theme
    arandr
    autocutsel
    autojump
    binutils
    chromium
    clang
    cmus
    dmenu2
    dunst
    emacs
    entr
    exa
    fd
    feh
    ffmpeg
    firefox
    fzf
    gimp
    git
    gitAndTools.diff-so-fancy
    gnome3.nautilus
    gnumake
    gnupg
    gparted
    haskellPackages.ghcid
    haskellPackages.xmobar
    hlint
    htop
    jq
    kakoune
    killall
    kitty
    libreoffice-fresh
    lsof
    lxappearance
    mpc_cli
    mpd
    mpv
    mupdf
    ncmpcpp
    neofetch
    neovim
    nixops
    nnn
    nodePackages.prettier
    nodejs
    notify-desktop
    pandoc
    papirus-icon-theme
    polybar
    powertop
    python3
    ranger
    rclone
    ripgrep
    rofi
    rsync
    rustup
    scrot
    shellcheck
    slack
    spotify
    stack
    stow
    sxiv
    tealdeer
    tectonic
    tmux
    transmission-gtk
    unar
    unzip
    xclip
    xorg.xev
    xorg.xrdb
    xorg.xset
    xst
    youtube-dl
    zathura
    zip

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
      roboto
      dejavu_fonts
      liberation_ttf
      libertinus

      # Monospaced
      iosevka-bin
      inconsolata

      # Bitmap
      terminus_font

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
  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    sensitivity = 100; # 0-255 (128)
    speed = 80; # 0-255 (97)
  };
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
    desktopManager = {
      default = "none";
      xterm.enable = false;
      xfce = {
        enable = true;
        enableXfwm = false;
        noDesktop = true;
      };
    };
    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    displayManager.lightdm.enable = true;
    xautolock = {
      enable = true;
      time = 5;
      locker = "${pkgs.systemd}/bin/systemctl suspend";
      enableNotifier = true;
      notify = 10;
      notifier = "${pkgs.notify-desktop}/bin/notify-desktop 'Locking in 10 seconds...'";
    };
  };
  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "opengl";
  };
  services.redshift = {
    enable = true;
    provider = "geoclue2";
    temperature.day = 6500;
    temperature.night = 4000;
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
  services.physlock = {
    enable = true;
    lockOn.hibernate = false;
  };


  # NETWORK {{{1
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };
  networking.hostName = "x1";
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };
  services.avahi = {
    enable = true;
    nssmdns = true;
  };


  # BACKUP {{{1
  services.syncthing = {
    enable = true;
    user = "evanrelf";
    dataDir = "/home/evanrelf/.config/syncthing";
    openDefaultPorts = true;
  };
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
    extraGroups = [
      "audio"
      "docker"
      "networkmanager"
      "sway"
      "wheel"
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
    initrd.luks.devices = [
      {
        name = "root";
        device = "/dev/disk/by-uuid/76d089cb-ab13-43c2-a63e-cdccd2f56941";
        preLVM = true;
        allowDiscards = true;
      }
    ];
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
