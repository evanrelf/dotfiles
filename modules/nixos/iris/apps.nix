{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.iris.apps;

  socketApps = lib.filterAttrs (_: app: app.port == null) cfg;

  portApps = lib.filterAttrs (_: app: app.port != null) cfg;

in
{
  options.iris.apps = lib.mkOption {
    default = { };
    type = lib.types.attrsOf (lib.types.submodule {
      options = {
        package = lib.mkOption { type = lib.types.package; };
        port = lib.mkOption {
          type = lib.types.nullOr lib.types.port;
          default = null;
          description = "Listen on this port instead of a Unix socket";
        };
        onDemand = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Start on the first request instead of at boot, and let the app exit when it's done";
        };
      };
    });
  };

  config = {
    assertions =
      lib.mapAttrsToList
        (name: app: {
          assertion = !(app.onDemand && app.port != null);
          message = "iris.apps.${name}: `onDemand` requires a Unix socket, so it can't be used with `port`";
        })
        cfg
      ++ lib.mapAttrsToList
        (port: names: {
          assertion = lib.length names == 1;
          message = "iris.apps: Port ${port} is used by multiple apps: ${lib.concatStringsSep ", " names}";
        })
        (lib.groupBy (name: toString cfg.${name}.port) (lib.attrNames portApps));

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

    systemd.sockets =
      lib.mapAttrs'
        (name: _: lib.nameValuePair "app-${name}" {
          wantedBy = [ "sockets.target" ];
          listenStreams = [ "/run/app-${name}.sock" ];
          socketConfig = {
            SocketUser = "root";
            SocketGroup = config.services.nginx.group;
            SocketMode = "0660";
          };
        })
        socketApps;

    systemd.services =
      lib.mapAttrs'
        (name: app: lib.nameValuePair "app-${name}" (lib.mkMerge [
          {
            wantedBy = lib.mkIf (!app.onDemand) [ "multi-user.target" ];
            serviceConfig = {
              ExecStart = lib.getExe app.package;
              DynamicUser = true;
              StateDirectory = "app-${name}"; # in /var/lib/
              Restart = if app.onDemand then "no" else "always";
            };
          }
          (if app.port == null then {
            requires = [ "app-${name}.socket" ];
            after = [ "app-${name}.socket" ];
          } else {
            environment = { PORT = toString app.port; };
          })
          (lib.mkIf app.onDemand {
            environment = { ON_DEMAND = "true"; };
          })
        ]))
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
            proxyPass =
              if app.port == null
              then "http://unix:/run/app-${name}.sock"
              else "http://127.0.0.1:${toString app.port}";
            recommendedProxySettings = true;
          };
        })
        cfg;
  };
}
