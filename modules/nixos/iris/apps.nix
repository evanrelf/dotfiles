{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.iris.apps;

  defaultIdleTimeout = "5min";

  waitForPort = port:
    pkgs.writeShellScript "wait-for-port-${toString port}" ''
      until (exec 3<>/dev/tcp/127.0.0.1/${toString port}) 2>/dev/null; do
        sleep 0.1
      done
    '';

in
{
  options.iris.apps = lib.mkOption {
    default = { };
    type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
      options = {
        command = lib.mkOption {
          type = lib.types.str;
          example = lib.literalExpression ''"''${lib.getExe pkgs.my-server} launch"'';
          description = "Command line to run the app, used as `ExecStart` (not a shell, but `$PORT` is expanded)";
        };
        port = lib.mkOption {
          type = lib.types.port;
          default = 20000 + lib.mod (lib.fromHexString (lib.substring 0 7 (builtins.hashString "sha256" name))) 10000;
          defaultText = "derived from the app's name";
          description = "Port the app listens on, passed as $PORT";
        };
        onDemand = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Start on the first request instead of at boot, and stop after `idleTimeout` without connections";
        };
        idleTimeout = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          example = "30s";
          description = "How long an on-demand app can run without connections before it's stopped (default ${defaultIdleTimeout})";
        };
      };
    }));
  };

  config = {
    assertions =
      lib.mapAttrsToList
        (name: app: {
          assertion = app.idleTimeout != null -> app.onDemand;
          message = "iris.apps.${name}: `idleTimeout` requires `onDemand = true`";
        })
        cfg
      ++ lib.mapAttrsToList
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

    systemd.sockets =
      lib.mapAttrs'
        (name: _: lib.nameValuePair "app-${name}-proxy" {
          wantedBy = [ "sockets.target" ];
          listenStreams = [ "/run/app-${name}.sock" ];
          socketConfig = {
            SocketUser = "root";
            SocketGroup = config.services.nginx.group;
            SocketMode = "0660";
          };
        })
        (lib.filterAttrs (_: app: app.onDemand) cfg);

    systemd.services =
      lib.concatMapAttrs
        (name: app: {
          "app-${name}" = {
            wantedBy = lib.mkIf (!app.onDemand) [ "multi-user.target" ];
            environment.PORT = toString app.port;
            unitConfig.StopWhenUnneeded = lib.mkIf app.onDemand true;
            serviceConfig = {
              ExecStart = app.command;
              # Don't let the proxy forward connections until the app is listening
              ExecStartPost = lib.mkIf app.onDemand (waitForPort app.port);
              Restart = if app.onDemand then "no" else "always";
              StateDirectory = "app-${name}"; # in /var/lib/
              DynamicUser = true;
            };
          };
        } // lib.optionalAttrs app.onDemand {
          "app-${name}-proxy" = {
            bindsTo = [ "app-${name}.service" ];
            after = [ "app-${name}.service" ];
            serviceConfig = {
              ExecStart = lib.escapeShellArgs [
                "${config.systemd.package}/lib/systemd/systemd-socket-proxyd"
                "--exit-idle-time=${if app.idleTimeout != null then app.idleTimeout else defaultIdleTimeout}"
                "127.0.0.1:${toString app.port}"
              ];
              DynamicUser = true;
            };
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
            proxyPass =
              if app.onDemand
              then "http://unix:/run/app-${name}.sock"
              else "http://127.0.0.1:${toString app.port}";
            recommendedProxySettings = true;
          };
        })
        cfg;
  };
}
