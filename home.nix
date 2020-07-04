let
  common = { pkgs, ... }: {
    imports = import ./modules/layers;

    home.stateVersion = "20.03";

    news.display = "silent";

    manual.html.enable = true;

    nixpkgs.overlays = import ./overlays;

    layers.fish.enable = true;

    layers.git.enable = true;

    layers.haskell.enable = true;

    layers.kakoune.enable = true;

    layers.kitty.enable = true;

    layers.neovim.enable = true;

    layers.nix.enable = true;
    # layers.nix.declarativeChannels =
    #   let
    #     revisions = {
    #       "nixos-20.03" = {
    #         # ↓ Don't change this revision often
    #         rev = "5adf2a6c11646898742b0c08f7e94101620ba707";
    #         sha256 = "0wf7pwma2qyfak39b242mcq8z7cdj65sds7hcjxchy0448shapzi";
    #       };
    #       "nixpkgs-unstable" = {
    #         rev = "c27e54de99df793756a5314f8fd5dd3e49d31927";
    #         sha256 = "0289m555hpmml5g6idicg1bckphww22p87s8qca2k046zkz0ykx0";
    #       };
    #       "master" = {
    #         rev = "a48adb351116f1a0ab81fe551a36436e447cbfff";
    #         sha256 = "15jhg21b5an94himdrc93h55zr943miw070mprn3wxhv9b5n864b";
    #       };
    #     };
    #     aliases = rec {
    #       stable = revisions."nixos-20.03";
    #       unstable = revisions."nixpkgs-unstable";
    #       master = revisions."master";
    #       default = unstable;
    #     };
    #   in
    #     revisions // aliases;

    layers.tmux.enable = true;

    home.packages = with pkgs; [
      (aspellWithDicts (d: with d; [ en en-computers en-science ]))
      (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
      dhall
      dhall-json
      exa
      fd
      fzf
      gcoreutils
      htop
      httpie
      jq
      moreutils
      pandoc
      python3
      ripgrep
      sd
      shellcheck
      tealdeer
      tokei
      universal-ctags
    ];
  };

  personal = { pkgs, ... }: {
    layers.fun.enable = true;

    home.packages = with pkgs; [
      ffmpeg-full
      rustup
      youtube-dl
    ];
  };

  darwin = { ... }: {
    layers.hammerspoon.enable = true;

    layers.karabiner.enable = true;
  };

  linux = { lib, ... }: {
    home.file =
      let
        enabled = [
          # "nixos"
          "sway"
          # "xmonad"
          "xorg"
        ];
        configs =
          lib.filterAttrs
            (name: _: builtins.elem name enabled)
            (builtins.readDir ./files);
        symlink = name: _: {
          source = ./files + "/${name}";
          target = ".";
          recursive = true;
        };
      in
        lib.mapAttrs symlink configs;
  };

in
  {
    auburn = { ... }: { imports = [ common personal darwin ]; };
    indigo = { ... }: { imports = [ common darwin ]; };
    sienna = { ... }: { imports = [ common personal linux ]; };
  }
