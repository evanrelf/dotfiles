# Self-hosted Amp orbs (https://ampcode.com/docs/orbs/self-hosted), run as
# rootless Podman containers owned by an unprivileged `amp` user.
#
# Every container the provider can see is an orb, so the dispatch program is
# Amp's documented Docker example with `podman` instead of `docker`. Root inside
# an orb maps to `amp` on the host, and `amp` can't read other users' homes.
#
# Orbs get no bind mounts, so they can't see or write to the host's
# filesystem. They can reach the internet, but not the host, the LAN, or the
# tailnet.

{ config, lib, pkgs, ... }:

let
  cfg = config.services.amp-orbs;

  # Fixed so the user's slice can be configured below. NixOS allocates system
  # UIDs from 999 downwards, and static UIDs stay below 400.
  uid = 400;

  home = "/var/lib/amp";

  # Destinations that nothing in the `amp` user's slice can reach. Loopback
  # isn't blocked, so the provider can use the host's DNS resolver; orbs can't
  # reach the host's loopback because of the pasta options below.
  blockedNetworks = [
    # IPv4
    "0.0.0.0/8" # "This network"; connecting to 0.0.0.0 reaches the host itself
    "10.0.0.0/8" # Private
    "100.64.0.0/10" # Carrier-grade NAT; Tailscale assigns tailnet IPs from here
    "169.254.0.0/16" # Link-local, including cloud metadata (169.254.169.254)
    "172.16.0.0/12" # Private; Docker's default bridges
    "192.168.0.0/16" # Private; home LANs, including iris's
    "224.0.0.0/4" # Multicast, like mDNS and UPnP discovery
    "240.0.0.0/4" # Reserved, including broadcast (255.255.255.255)
    # IPv6
    "fc00::/7" # Unique local (private); includes Tailscale's fd7a:115c:a1e0::/48
    "fe80::/10" # Link-local
    "ff00::/8" # Multicast
  ];

  # Marks images saved as project snapshots, so maintenance can prune them
  snapshotLabel = "amp.snapshot=true";

  dispatch = pkgs.writeShellApplication {
    name = "amp-orb-dispatch";
    runtimeInputs = [ config.virtualisation.podman.package ];
    text = ''
      # No port forwarding in either direction, and no address that maps to
      # the host's loopback. Podman passes these by default; pin them anyway.
      pasta_options=--ipv4-only,-t,none,-u,none,-T,none,-U,none,--no-map-gw

      # The slice's IP filter applies inside orbs too. It blocks the link-local
      # address Podman would forward DNS through, so use public resolvers.
      dns_options=(--dns 1.1.1.1 --dns 1.0.0.1)

      case "$ORB_OPERATION" in
        spawn)
          image="$ORB_IMAGE"
          if [[ -n ''${ORB_SNAPSHOT_ID-} ]] && podman image exists "$ORB_SNAPSHOT_ID"; then
            image="$ORB_SNAPSHOT_ID"
          fi
          if podman container exists "$ORB_ID"; then
            podman start "$ORB_ID"
          else
            podman run \
              --detach \
              --init \
              --platform linux/amd64 \
              --name "$ORB_ID" \
              --network "pasta:''${pasta_options}" \
              "''${dns_options[@]}" \
              --cpus ${toString cfg.cpus} \
              --memory ${cfg.memory} \
              --pids-limit ${toString cfg.pidsLimit} \
              "$image" \
              sleep infinity
          fi
          env_args=(--env ORB_TOKEN)
          if [[ -n ''${AMP_URL-} ]]; then
            env_args+=(--env AMP_URL)
          fi
          podman exec --detach "''${env_args[@]}" "$ORB_ID" amp orb bootstrap
          ;;
        pause)
          if podman container exists "$ORB_ID"; then
            podman stop "$ORB_ID"
            if [[ -n ''${ORB_SNAPSHOT_ID-} ]]; then
              podman commit --change "LABEL ${snapshotLabel}" "$ORB_ID" "$ORB_SNAPSHOT_ID"
            fi
          fi
          ;;
        delete)
          podman rm --force --ignore --volumes "$ORB_ID"
          ;;
        *)
          echo "unknown operation: $ORB_OPERATION" >&2
          exit 1
          ;;
      esac
    '';
  };

  # Where maintenance builds the latest amp
  ampLink = "${home}/amp";

  maintenance = pkgs.writeShellApplication {
    name = "amp-orb-maintenance";
    runtimeInputs = [
      config.nix.package
      config.systemd.package
      config.virtualisation.podman.package
      pkgs.coreutils
      pkgs.getent
    ];
    text = ''
      # User services can't wait for network-online.target, so give the
      # network up to five minutes after boot
      for _ in {1..60}; do
        if getent hosts github.com >/dev/null; then
          break
        fi
        sleep 5
      done

      # Keep going after a failed step, but report it
      failed=0

      ${lib.optionalString (cfg.updateFrom != null) ''
        # Amp drops support for old versions quickly, so build the latest
        # rather than waiting for this flake's lock to be bumped
        old=$(readlink -f ${ampLink} || true)
        nix build --out-link ${ampLink} ${lib.escapeShellArg cfg.updateFrom} || failed=1
        if [[ $(readlink -f ${ampLink} || true) != "$old" ]]; then
          # Don't wait, because the provider starts after this service
          systemctl --user --no-block try-restart amp-orb-provider.service || failed=1
        fi
      ''}

      podman pull ${lib.escapeShellArg cfg.image} || failed=1
      # Old versions of the orb image, untagged by the pull above
      podman image prune --force || failed=1
      # Snapshots that no orb is using, saved over a week ago. If Amp asks for
      # one later, the orb starts from the image and runs `.agents/setup`.
      podman image prune --all --force --filter label=${snapshotLabel} --filter until=168h || failed=1

      exit "$failed"
    '';
  };

in
{
  options.services.amp-orbs = {
    enable = lib.mkEnableOption "self-hosted Amp orbs";

    package = lib.mkPackageOption pkgs [ "llm-agents" "amp" ] { };

    updateFrom = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "github:numtide/llm-agents.nix#amp";
      description = ''
        Flake output that maintenance rebuilds amp from every day. The provider
        uses `package` until the first successful build. Set to `null` to only
        use `package`.
      '';
    };

    tokenFile = lib.mkOption {
      type = lib.types.str;
      example = "/persist/etc/amp/orb-provider-token";
      description = ''
        File containing the orb provider token (`aop_live_…`) from workspace
        settings. Must be readable by the `amp` user.
      '';
    };

    image = lib.mkOption {
      type = lib.types.str;
      default = "us-central1-docker.pkg.dev/amp-prod-0/amp-orb/amp-orb:latest";
      description = "Image that new orbs start from. Pulled at boot and daily.";
    };

    cpus = lib.mkOption {
      type = lib.types.ints.positive;
      default = 4;
      description = "CPUs available to each orb.";
    };

    memory = lib.mkOption {
      type = lib.types.str;
      default = "8g";
      description = "Memory limit for each orb, in Podman's format.";
    };

    pidsLimit = lib.mkOption {
      type = lib.types.ints.positive;
      default = 4096;
      description = "Process limit for each orb.";
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman.enable = true;

    users.users.amp = {
      inherit uid;
      isSystemUser = true;
      group = "amp";
      inherit home;
      createHome = true;
      autoSubUidGidRange = true;
      # Start the user's systemd instance at boot, without a login
      linger = true;
    };

    users.groups.amp = { };

    systemd.slices."user-${toString uid}" = {
      overrideStrategy = "asDropin";
      sliceConfig.IPAddressDeny = blockedNetworks;
    };

    # A user service, so Podman can put each orb in its own scope under the
    # user's systemd instance. `nixos-rebuild switch` doesn't restart user
    # services, so after changing this module, run:
    #   sudo systemctl --user -M amp@ restart amp-orb-provider
    systemd.user.services.amp-orb-provider = {
      description = "Amp orb provider";
      wantedBy = [ "default.target" ];
      # Start after maintenance, so the provider runs the latest amp, and the
      # first spawn doesn't have to pull the image within Amp's five-minute
      # limit
      after = [ "amp-orb-maintenance.service" ];
      unitConfig = {
        ConditionUser = "amp";
        ConditionPathExists = cfg.tokenFile;
      };
      environment.ORB_IMAGE = cfg.image;
      script = ''
        amp=${lib.getExe cfg.package}
        ${lib.optionalString (cfg.updateFrom != null) ''
          if [[ -x ${ampLink}/bin/amp ]]; then
            amp=${ampLink}/bin/amp
          fi
        ''}
        AMP_ORB_PROVIDER_TOKEN=$(< ${lib.escapeShellArg cfg.tokenFile})
        export AMP_ORB_PROVIDER_TOKEN
        exec "$amp" orb provider ${lib.getExe dispatch}
      '';
      serviceConfig = {
        # Don't take orbs down with the provider when it restarts
        KillMode = "process";
        Restart = "always";
        RestartSec = 10;
      };
    };

    systemd.user.services.amp-orb-maintenance = {
      description = "Amp orb maintenance";
      wantedBy = [ "default.target" ];
      unitConfig.ConditionUser = "amp";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe maintenance;
        TimeoutStartSec = "30min";
      };
    };

    systemd.user.timers.amp-orb-maintenance = {
      wantedBy = [ "timers.target" ];
      unitConfig.ConditionUser = "amp";
      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
      };
    };
  };
}
