{ inputs, withSystem, ... }:

let
  mkHomeConfiguration = system: modules:
    withSystem system ({ pkgs, ... }:
      inputs.nixpkgs.lib.fix (self:
        (inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs modules;
          extraSpecialArgs = { inherit inputs; };
        }) // { default = self.activationPackage; }
      )
    );

in
{
  flake.homeConfigurations = {
    iris =
      mkHomeConfiguration "x86_64-linux" [ ../home/machines/iris.nix ];

    lima =
      mkHomeConfiguration "aarch64-linux" [ ../home/machines/lima.nix ];

    pearl =
      mkHomeConfiguration "aarch64-darwin" [ ../home/machines/pearl.nix ];

    ultraviolet =
      mkHomeConfiguration "aarch64-darwin" [ ../home/machines/ultraviolet.nix ];
  };
}
