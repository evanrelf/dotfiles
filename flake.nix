{
  description = "evanrelf's dotfiles";

  inputs = {
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
    tpm = {
      url = "github:tmux-plugins/tpm";
      flake = false;
    };
    vim-plug = {
      url = "github:junegunn/vim-plug";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      eachSystem = f:
        builtins.listToAttrs
          (builtins.map
            (system: { name = system; value = f system; })
            [
              "aarch64-darwin"
              "x86_64-darwin"
              "x86_64-linux"
            ]);

      config = { };

      overlays = [
        # Make flake inputs available in overlays
        (_: _: { inherit inputs; })
        # Make packages for other systems available for cross compilation
        (_: _: eachSystem (system:
          import nixpkgs { inherit system config overlays; }
        ))
        # Use newer `home-manager` from flake input
        (_: { system, ... }: {
          inherit (home-manager.packages."${system}") home-manager;
        })
        inputs.emacs-overlay.overlay
        (import ./overlays/kakoune-plugins.nix)
        (import ./overlays/top-level.nix)
        (import ./overlays/home-configurations.nix)
      ];

    in
    {
      defaultPackage = eachSystem (system: self.packages."${system}".dotfiles);

      packages = eachSystem (system:
        import nixpkgs { inherit system config overlays; }
      );
    };
}
