{ config, inputs, withSystem, ... }:

{
  flake.darwinConfigurations.pearl =
    withSystem "aarch64-darwin" ({ pkgs, ... }:
      inputs.nix-darwin.lib.darwinSystem {
        inherit pkgs;
        modules = [ config.flake.darwinModules.pearl ];
        specialArgs = { inherit inputs; };
      }
    );

  flake.darwinModules.pearl = { config, ... }: {
    imports = [
      ../../darwin/common.nix
    ];

    # TODO: Reinstall Lix.
    ids.gids.nixbld = 30000;

    # TODO: This machine still has the Determinate Nix installer and its repair
    # launch daemon (`systems.determinate.nix-installer.nix-hook`), which prepends
    # text to shell profiles on reboot.
    #
    # For now, these hashes tell `nix-darwin` these modifications are okay to
    # overwrite. In the future, it would be better to clean up this hodgepodge of
    # Nix installs so this hack isn't necessary.
    environment.etc."bashrc".knownSha256Hashes = [ "fc15d82a792799f14431e643b35fd9f6b07b374ed0509ceb14052f05fe664b8a" ];
    environment.etc."zshrc".knownSha256Hashes = [ "27274e44b88a1174787f9a3d437d3387edc4f9aaaf40356054130797f5dc7912" ];
    environment.etc."zshenv".knownSha256Hashes = [ "ac3d39febe5ffcd0f404dda281bea91c6a2ec9870896276989baa00fd2bb23a6" ];

    system.stateVersion = 6;
  };

  flake.homeConfigurations.pearl =
    withSystem "aarch64-darwin" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ config.flake.homeModules.pearl ];
        extraSpecialArgs = { inherit inputs; };
      }
    );

  flake.homeModules.pearl = { ... }: {
    imports = [
      ../../home/common.nix
    ];

    home.stateVersion = "22.11";
  };
}
