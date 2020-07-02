{ pkgs, ... }:

{ imports = [
    ./modules/layers/borg
    ./modules/layers/fish
    ./modules/layers/fun.nix
    ./modules/layers/git
    ./modules/layers/hammerspoon
    ./modules/layers/haskell
    ./modules/layers/kakoune
    ./modules/layers/karabiner
    ./modules/layers/kitty
    ./modules/layers/nix.nix
    ./modules/layers/nodejs
    ./modules/layers/tmux
  ];

  home.stateVersion = "20.03";

  news.display = "silent";

  manual.html.enable = true;

  nixpkgs.overlays = [ (import ./overlay.nix) ];

  layers.fish.enable = true;

  layers.fun.enable = true;

  layers.git.enable = true;

  layers.hammerspoon.enable = true;

  layers.haskell.enable = true;

  layers.kakoune.enable = true;

  layers.karabiner.enable = true;

  layers.kitty.enable = true;

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

  layers.nodejs.enable = true;

  layers.tmux.enable = true;

  home.packages = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
    coin
    dhall
    dhall-json
    exa
    fd
    ffmpeg-full
    fzf
    gcoreutils
    htop
    httpie
    jq
    moreutils
    neovim
    pandoc
    python3
    ripgrep
    rustup
    sd
    shellcheck
    tealdeer
    tokei
    universal-ctags
    youtube-dl
  ];
}
