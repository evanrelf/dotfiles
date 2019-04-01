#!/usr/bin/env bash

# Groups
AddPackageGroup base
AddPackageGroup base-devel

# Sway
AddPackage sway # Tiling Wayland compositor and replacement for the i3 window manager
AddPackage swayidle # Idle management daemon for Wayland
AddPackage swaylock # Screen locker for Wayland
AddPackage xorg-server-xwayland # run X clients under wayland

# Packages
AddPackage acpi # Client for battery, power, and thermal readings
AddPackage alsa-utils # An alternative implementation of Linux sound support
AddPackage alsa-utils # An alternative implementation of Linux sound support
AddPackage autocutsel # synchronizes the two copy/paste buffers mainly used by X applications
AddPackage autojump # A faster way to navigate your filesystem from the command line
AddPackage btrfs-progs # Btrfs filesystem utilities
AddPackage clang # C language family frontend for LLVM
AddPackage compton # X compositor that may fix tearing issues
AddPackage ctags # Generates an index file of language objects found in source files
AddPackage diff-so-fancy # Good-looking diffs with diff-highlight and more
AddPackage docker # Pack, ship and run any application as a lightweight container
AddPackage docker # Pack, ship and run any application as a lightweight container
AddPackage dunst # Customizable and lightweight notification-daemon
AddPackage entr # Run arbitrary commands when files change
AddPackage exa # ls replacement
AddPackage fd # Simple, fast and user-friendly alternative to find
AddPackage feh # Fast and light imlib2-based image viewer
AddPackage firefox # Standalone web browser from mozilla.org
AddPackage fish # Smart and user friendly shell intended mostly for interactive use
AddPackage fwupd # A simple daemon to allow session software to update firmware
AddPackage fzf # Command-line fuzzy finder
AddPackage hlint # Source code suggestions
AddPackage htop # Interactive process viewer
AddPackage intel-ucode # Microcode update files for Intel CPUs
AddPackage kitty # A modern, hackable, featureful, OpenGL based terminal emulator
AddPackage light # Program to easily change brightness on backlight-controllers.
AddPackage light # Program to easily change brightness on backlight-controllers.
AddPackage lvm2 # Logical Volume Manager 2 utilities
AddPackage lxappearance # Feature-rich GTK+ theme switcher of the LXDE Desktop
AddPackage materia-gtk-theme # A Material Design theme for GNOME/GTK+ based desktop environments
AddPackage mosh # Mobile shell, surviving disconnects with local echo and line editing
AddPackage mupdf # Lightweight PDF and XPS viewer
AddPackage neofetch # A CLI system information tool written in BASH that supports displaying images.
AddPackage neovim # Fork of Vim aiming to improve user experience, plugins, and GUIs
AddPackage networkmanager # Network connection manager and user applications
AddPackage nodejs # Evented I/O for V8 javascript
AddPackage noto-fonts-emoji # Google Noto emoji fonts
AddPackage npm # A package manager for javascript
AddPackage pandoc # Conversion between markup formats
AddPackage physlock # Lightweight Linux console locking tool
AddPackage powertop # A tool to diagnose issues with power consumption and power management
AddPackage pulseaudio # A featureful, general-purpose sound server
AddPackage pulseaudio # A featureful, general-purpose sound server
AddPackage pulseaudio-alsa # ALSA Configuration for PulseAudio
AddPackage pulseaudio-alsa # ALSA Configuration for PulseAudio
AddPackage redshift # Adjusts the color temperature of your screen according to your surroundings.
AddPackage ripgrep # A search tool that combines the usability of ag with the raw speed of grep
AddPackage rofi # A window switcher, application launcher and dmenu replacement
AddPackage rsync # A file transfer program to keep remote files in sync
AddPackage rustup # The Rust toolchain installer
AddPackage scrot # Simple command-line screenshot utility for X
AddPackage shellcheck # Shell script analysis tool
AddPackage stack # The Haskell Tool Stack
AddPackage stow # Manage installation of multiple softwares in the same directory tree
AddPackage sxiv # Simple X Image Viewer
AddPackage syncthing # Open Source Continuous Replication / Cluster Synchronization Thing
AddPackage time # Utility for monitoring a program's use of system resources
AddPackage tlp # Linux Advanced Power Management
AddPackage ttf-dejavu # Font family based on the Bitstream Vera Fonts with a wider range of characters
AddPackage ttf-inconsolata # Monospace font for pretty code listings and for the terminal
AddPackage ttf-roboto # Google's signature family of fonts
AddPackage unarchiver # unar and lsar
AddPackage unzip # For extracting and viewing files in .zip archives
AddPackage xautolock # An automatic X screen-locker/screen-saver
AddPackage xclip # Command line interface to the X11 clipboard
AddPackage xf86-video-intel # X.org Intel i810/i830/i915/945G/G965+ video drivers
AddPackage xmonad # Lightweight X11 tiled window manager written in Haskell
AddPackage xmonad-contrib # Add-ons for xmonad
AddPackage xorg-server # Xorg X server
AddPackage xorg-xinit # X.Org initialisation program
AddPackage xorg-xinput # Small commandline tool to configure devices
AddPackage xorg-xmessage # Display a message or query in a window
AddPackage xorg-xrandr # Primitive command line interface to RandR extension
AddPackage xorg-xrdb # X server resource database utility
AddPackage xorg-xset # User preference utility for X
AddPackage xorg-xsetroot # Classic X utility to set your root window background to a given pattern or color
AddPackage zathura # Minimalistic document viewer
AddPackage zip # Compressor/archiver for creating and modifying zipfiles

# AUR
AddPackage --foreign aconfmgr-git # A configuration manager for Arch Linux
AddPackage --foreign aura-git # A package manager for Arch Linux and its AUR
AddPackage --foreign chromium-vaapi-bin # Chromium with VA-API support to enable hardware acceleration, pre-compiled
AddPackage --foreign polybar # A fast and easy-to-use status bar
AddPackage --foreign pop-gtk-theme-bin # System76 Pop GTK+ Theme
AddPackage --foreign pop-icon-theme-bin # A free and open source SVG icon theme for Linux, based on Paper Icon Set and Papirus.
AddPackage --foreign spotify # A proprietary music streaming service
AddPackage --foreign tealdeer # An implementation of tldr in Rust
AddPackage --foreign tectonic # Modernized, complete, self-contained TeX/LaTeX engine, powered by XeTeX and TeXLive
AddPackage --foreign throttled # Workaround for Intel throttling issues in Linux.
AddPackage --foreign ttf-iosevka # A slender monospace typeface. Shape
AddPackage --foreign ttf-material-icons # Google Material Design icon font
AddPackage --foreign xbanish # Hide the mouse cursor when typing
AddPackage --foreign xst-git # st fork fork with xresources support and other patches
AddPackage --foreign yay # Yet another yogurt. Pacman wrapper and AUR helper written in go.

# Old
# AddPackage --foreign thermald # The Linux Thermal Daemon program from 01.org
