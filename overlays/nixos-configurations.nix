final: prev:

let
  mkNixOSConfiguration = system: modules:
    final.inputs.nixpkgs.lib.nixosSystem {
      inherit system modules;
      pkgs = final;
      specialArgs = { inherit (prev) inputs; };
    };

in
{
  # TODO:
  # * No internet access in VM.
  # * `microvm-run` isn't suitable for interactive use because it interprets
  #   SIGINT as poweroff. Need to SSH instead.

  microvm = final.nixosConfigurations.microvm.config.microvm.runner.vfkit;

  nixosConfigurations = {
    iris =
      mkNixOSConfiguration
        "x86_64-linux"
        [ ../modules/nixos/machines/iris/configuration.nix ];

    microvm =
      final.inputs.nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          final.inputs.microvm.nixosModules.microvm
          ({ lib, pkgs, ... }: {
            networking.hostName = "microvm";
            services.getty.autologinUser = "root";
            microvm = {
              hypervisor = "vfkit";
              shares = [{
                tag = "ro-store";
                source = "/nix/store";
                mountPoint = "/nix/.ro-store";
                proto = "virtiofs";
              }];
              interfaces = [{
                type = "user";
                id = "microvm";
                mac = "02:00:00:00:00:01";
              }];
              vmHostPackages =
                final.inputs.nixpkgs.legacyPackages.aarch64-darwin;
            };
            nixpkgs.overlays = [ final.inputs.self.overlays.default ];
            environment.systemPackages = with pkgs; [
              kakoune
            ];
            system.stateVersion = lib.trivial.release;
          })
        ];
      };

    vm =
      mkNixOSConfiguration
        "aarch64-linux"
        [ ../modules/nixos/machines/vm.nix ];

    vm-installer =
      mkNixOSConfiguration
        "aarch64-linux"
        [ ../modules/nixos/machines/vm-installer.nix ];
  };

  nixosImages = {
    vm-installer =
      final.inputs.nixos-generators.nixosGenerate {
        pkgs = final;
        modules = [ ../modules/nixos/machines/vm-installer.nix ];
        format = "install-iso";
        specialArgs = { inherit (prev) inputs; };
      };
  };
}
