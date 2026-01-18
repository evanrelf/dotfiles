{ config, lib, pkgs, ... }:

let
  tailscaleIPAddress = "100.101.235.127";

in
{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    "cryptroot1".device = "/dev/disk/by-uuid/cbe9b70b-75eb-491b-90a9-ac3edbb25efb";
    "cryptroot2".device = "/dev/disk/by-uuid/3072a49f-29f1-465a-b516-cbe6e4ce755f";
  };

  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "babed00d";
  boot.zfs.devNodes = "/dev/mapper";
  services.zfs.autoScrub.enable = true;

  # TODO: Re-enable "erase your darlings"
  # boot.initrd.postDeviceCommands = lib.mkAfter ''
  #   zfs rollback -r tank/local/root@blank
  # '';

  networking.hostName = "iris";

  networking.networkmanager.enable = true;

  time.timeZone = "America/Los_Angeles";

  users.users.evanrelf = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    initialPassword = "banana";
    shell = "/home/evanrelf/.nix-profile/bin/fish";
  };

  environment.systemPackages = with pkgs; [
    git
    kakoune
    neovim
  ];

  # Persist state
  environment.etc = {
    "nixos".source = "/persist/etc/nixos";
    "NetworkManager/system-connections".source = "/persist/etc/NetworkManager/system-connections";
    "adjtime".source = "/persist/etc/adjtime";
    "NIXOS".source = "/persist/etc/NIXOS";
    "machine-id".source = "/persist/etc/machine-id";
    "miniflux/admin-credentials".source = "/persist/etc/miniflux/admin-credentials";
  };

  systemd.tmpfiles.rules = [
    "L /var/lib/NetworkManager/secret_key - - - - /persist/var/lib/NetworkManager/secret_key"
    "L /var/lib/NetworkManager/seen-bssids - - - - /persist/var/lib/NetworkManager/seen-bssids"
    "L /var/lib/NetworkManager/timestamps - - - - /persist/var/lib/NetworkManager/timestamps"
  ];

  security.sudo = {
    wheelNeedsPassword = false;
    extraConfig = ''
      Defaults lecture = never
    '';
  };

  services.openssh.enable = true;

  services.tailscale = {
    enable = true;
    extraSetFlags = [ "--ssh" ];
  };

  services.dnsmasq = {
    enable = true;
    settings.address = [
      "/iris.internal/${tailscaleIPAddress}"
      "/miniflux.iris.internal/${tailscaleIPAddress}"
      "/rss-bridge.iris.internal/${tailscaleIPAddress}"
    ];
    /* NOTE: Unnecessary because I'm using Tailscale split DNS
    settings.server = [
      "1.1.1.1"
      "1.0.0.1"
      "2606:4700:4700::1111"
      "2606:4700:4700::1001"
    ];
    */
  };

  services.miniflux = {
    enable = true;
    adminCredentialsFile = "/etc/miniflux/admin-credentials";
    config.LISTEN_ADDR = "0.0.0.0:10001";
    config.BASE_URL = "http://miniflux.iris.internal";
  };
  services.rss-bridge = {
    enable = true;
    config = {
      system.enabled_bridges = [ "CssSelectorBridge" ];
      error.output = "http";
      error.report_limit = 5;
    };
  };
  services.nginx = {
    enable = true;
    virtualHosts."miniflux.iris.internal" = {
      listen = [ { addr = "0.0.0.0"; port = 80; } ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:10001";
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
    };
    virtualHosts."rss-bridge.iris.internal" = {
      listen = [ { addr = "0.0.0.0"; port = 80; } ];
      locations."/" = {
        proxyPass = "http://127.0.0.1";
        extraConfig = ''
          proxy_set_header Host rss-bridge;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
    };
  };
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ 80 ];

  nix.package = pkgs.lixPackageSets.latest.lix;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.11"; # Did you read the comment?
}
