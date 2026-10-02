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
        jobs = lib.mkOption {
          default = { };
          description = "Background jobs, run as `app-<app>-job-<job>.service` with the app's user and state";
          type = lib.types.attrsOf (lib.types.submodule {
            options = {
              command = lib.mkOption {
                type = lib.types.str;
                example = lib.literalExpression ''"''${lib.getExe pkgs.my-server} reticulate-splines"'';
                description = "Command line to run the job, used as `ExecStart` (not a shell)";
              };
              startAt = lib.mkOption {
                type = lib.types.either lib.types.str (lib.types.listOf lib.types.str);
                default = [ ];
                example = "hourly";
                description = "When to run the job, in `systemd.time` calendar syntax (manual-only if empty)";
              };
            };
          });
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
        appsDir = ../../../../apps;
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
              # Shared with the app's jobs, so they can all access `$STATE_DIRECTORY`
              User = "app-${name}";
            };
          };
        } // lib.mapAttrs'
          (job: jobCfg: lib.nameValuePair "app-${name}-job-${job}" {
            inherit (jobCfg) startAt;
            serviceConfig = {
              Type = "oneshot";
              ExecStart = jobCfg.command;
              StateDirectory = "app-${name}";
              DynamicUser = true;
              User = "app-${name}";
            };
          })
          app.jobs
        // lib.optionalAttrs app.onDemand {
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

    # Catch up on runs missed while the machine was off
    systemd.timers =
      lib.concatMapAttrs
        (name: app:
          lib.mapAttrs'
            (job: _: lib.nameValuePair "app-${name}-job-${job}" {
              timerConfig.Persistent = true;
            })
            (lib.filterAttrs (_: jobCfg: lib.toList jobCfg.startAt != [ ]) app.jobs)
        )
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
