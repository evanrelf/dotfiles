{ config, pkgs, ... }:

{
  services.smartd.enable = true;
  environment.systemPackages = [ pkgs.smartmontools ];

  services.prometheus = {
    enable = true;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [
          "cpu"
          "cpufreq"
          "diskstats"
          "filesystem"
          "hwmon"
          "loadavg"
          "meminfo"
          "netdev"
          "netstat"
          "nvme"
          "thermal_zone"
          "time"
          "uname"
          "zfs"
        ];
      };
      systemd.enable = true;
    };
    scrapeConfigs = [
      {
        job_name = "iris";
        scrape_interval = "10s";
        static_configs = [
          {
            targets = [
              "localhost:${toString config.services.prometheus.exporters.node.port}"
              "localhost:${toString config.services.prometheus.exporters.systemd.port}"
            ];
          }
        ];
      }
    ];
  };

  services.grafana = {
    enable = true;
    addr = "0.0.0.0";
    auth.anonymous = {
      enable = true;
      org_role = "Editor";
    };
    provision = {
      enable = true;
      datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          url = "http://localhost:${toString config.services.prometheus.port}";
          isDefault = true;
        }
      ];
      dashboards = [
        { name = "dashboards"; options.path = ./grafana-dashboards; }
      ];
    };
    analytics.reporting.enable = false;
  };
}
