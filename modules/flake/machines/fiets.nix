{ config, inputs, withSystem, ... }:

{
  flake.homeConfigurations.fiets =
    withSystem "aarch64-darwin" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ config.flake.homeModules.fiets ];
        extraSpecialArgs = { inherit inputs; };
      }
    );

  flake.homeModules.fiets = { pkgs, ... }: {
    imports = [
      ../../home/common.nix
    ];

    home.packages = with pkgs; [
      llm-agents.claude-code
      llm-agents.pi
    ];

    home.stateVersion = "26.05";
  };
}
