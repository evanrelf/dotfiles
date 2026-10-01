{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.iris.apps;

in
{
  options.iris.apps = lib.mkOption {
    default = { };
    type = lib.types.attrsOf (lib.types.submodule ({ ... }: {
      options = {
        package = lib.mkOption { type = lib.types.package; };
        port = lib.mkOption { type = lib.types.port; };
      };
    }));
  };

  config = {
    assertions =
      lib.mapAttrsToList
        (port: names: {
          assertion = lib.length names == 1;
          message = "iris.apps: Port ${port} is used by multiple apps: ${lib.concatStringsSep ", " names}";
        })
        (lib.groupBy (name: toString cfg.${name}.port) (lib.attrNames cfg));

    iris.apps =
      let
        appsDir = ../../../apps;
      in
      lib.mapAttrs
        (name: _: import (appsDir + "/${name}") { inherit inputs lib pkgs; })
        (lib.filterAttrs
          (name: type:
            type == "directory" &&
            builtins.pathExists (appsDir + "/${name}/default.nix")
          )
          (builtins.readDir appsDir));

    systemd.services =
      lib.mapAttrs'
        (name: app: lib.nameValuePair "app-${name}" {
          wantedBy = [ "multi-user.target" ];
          environment = { PORT = toString app.port; };
          serviceConfig = {
            ExecStart = lib.getExe app.package;
            DynamicUser = true;
            StateDirectory = "app-${name}"; # in /var/lib/
            Restart = "on-failure";
          };
        })
        cfg;

    services.nginx.virtualHosts =
      lib.mapAttrs'
        (name: app: lib.nameValuePair "${name}.internal.evanrelf.com" {
          useACMEHost = "internal.evanrelf.com";
          forceSSL = true;
          listen = [
            { addr = "0.0.0.0"; port = 80; }
            { addr = "0.0.0.0"; port = 443; ssl = true; }
          ];
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString app.port}";
            recommendedProxySettings = true;
          };
        })
        cfg;
  };
}
