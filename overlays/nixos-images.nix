final: prev:

let
  vmInstaller =
    let
      nixosSystem = config:
        final.inputs.nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = final;
          modules = [ config ];
        };

      config = ../modules/nixos/machines/vm.nix;

      system = nixosSystem config;

      installerConfig = { modulesPath, ... }: {
        imports = [
          "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
        ];

        nix.settings.extra-experimental-features = [ "nix-command" "flakes" ];

        # Bake dependencies into ISO (makes it HUGE)
        # isoImage.storeContents = [ system.config.system.build.toplevel ];
        # isoImage.includeSystemBuildDependencies = true;
        boot.supportedFilesystems = [ "zfs" ];
        system.nixos-generate-config.configuration =
          builtins.replaceStrings [ "@" ] [ "\\@" ] (builtins.readFile config);

        # Install automatically
        environment.systemPackages = [ vm-install ];
        environment.loginShellInit = ''
          if [ "$(whoami)" = "nixos" ] && [ ! -e .tried-vm-install ]; then
            touch .tried-vm-install
            sudo vm-install
          fi
        '';
      };

      installerSystem = nixosSystem installerConfig;
    in
    installerSystem.config.system.build.isoImage;

  # Modified version of my "Install NixOS on ZFS With Opt-In State" gist:
  # https://gist.github.com/evanrelf/562102d6e8bc5b0f386fe8e91c40e863
  vm-install = final.writeShellScriptBin "vm-install" ''
    set -Eeuxo pipefail
    IFS=$'\n\t'

    DISK=/dev/nvme0n1
    BOOT=/dev/nvme0n1p1
    ROOT=/dev/nvme0n1p2

    # Clear disk
    wipefs --all --force $DISK
    sgdisk --zap-all --clear $DISK

    # Create GPT partition table
    parted --script $DISK -- mklabel gpt

    # Create boot partition
    parted --script $DISK -- mkpart ESP fat32 1MiB 512MiB
    parted --script $DISK -- set 1 boot on

    # Create root partition
    parted --script $DISK -- mkpart primary 512MiB 100%

    # Create ZFS storage pool for root partition
    zpool create -f -R /mnt -O mountpoint=none -O compression=zstd -O atime=off -O xattr=sa -O acltype=posixacl tank $ROOT

    # Create root ZFS dataset
    zfs create -p -o mountpoint=legacy tank/local/root

    # Create blank snapshot of root ZFS dataset
    zfs snapshot tank/local/root@blank

    # Mount root ZFS dataset
    mount -t zfs tank/local/root /mnt

    # Format boot partition as FAT32
    echo y | mkfs.fat -n boot -F 32 $BOOT

    # Mount boot partition
    mkdir /mnt/boot
    mount $BOOT /mnt/boot

    # Create and mount dataset for `/nix`
    zfs create -p -o mountpoint=legacy tank/local/nix
    mkdir /mnt/nix
    mount -t zfs tank/local/nix /mnt/nix

    # Create and mount dataset for `/home`
    zfs create -p -o mountpoint=legacy tank/safe/home
    mkdir /mnt/home
    mount -t zfs tank/safe/home /mnt/home

    # Create and mount dataset for persisted state
    zfs create -p -o mountpoint=legacy tank/safe/persist
    mkdir /mnt/persist
    mount -t zfs tank/safe/persist /mnt/persist

    # Generate initial NixOS configuration
    nixos-generate-config --root /mnt

    # Install NixOS
    nixos-install --no-root-passwd

    poweroff
  '';
in
{
  nixosImages = (prev.nixosImages or { }) // {
    inherit vmInstaller;
  };
}
