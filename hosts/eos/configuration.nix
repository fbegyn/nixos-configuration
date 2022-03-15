# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./acme.nix

    ../../common

    ../../users
    ../../users/francis

    # services
    # ../../services/coredns
    ../../services/adguard
    ../../services/ddclient
    ../../services/prometheus
    ../../services/grafana
    ../../services/node-exporter
    ../../services/tailscale.nix
    # ../../services/plex.nix
    ../../services/postgres
    ../../services/redis
    ../../services/consul
  ];

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  # disable the laptop lid switch
  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";
  services.resolved.extraConfig = ''
    DNSStubListener=no
  '';

  virtualisation.docker.enable = true;
  services.jupyterhub = {
    enable = false;
    jupyterlabEnv = pkgs.python3.withPackages (p: with p; [
      jupyterhub
      jupyterlab
      dockerspawner
    ]);
    jupyterhubEnv = pkgs.python3.withPackages (p: with p; [
      jupyterhub
      jupyterlab
      dockerspawner
    ]);
    spawner = "dockerspawner.DockerSpawner";
    extraConfig = ''
      c.JupyterHub.hub_ip = '10.5.1.10'
      c.Authenticator.allowed_users = { "francis" }
      c.Authenticator.admin_users = { "francis" }

      docker_notebook_dir = '/home/jovyan/work'
      c.DockerSpawner.image = 'jupyter/datascience-notebook:845d8ab3cd9d'
      c.DockerSpawner.hub_connect_ip = '10.5.1.10'
      c.DockerSpawner.notebook_dir = docker_notebook_dir
      c.DockerSpawner.volumes = { '/home/{username}/jupyterhub': docker_notebook_dir }
      c.DockerSpawner.remove_containers = True
      c.DockerSpawner.remove = True
    '';
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers = {
      hass = { volumes = [ "/home/francis/hass:/config" ];
      environment.TZ = "Europe/Brussels";
        image = "ghcr.io/home-assistant/home-assistant:stable";
        extraOptions = [
          "--network=host"
        ];
      };
      netbox = {
        image = "netboxcommunity/netbox:v3.1";
        ports = [
          "8080:8080"
        ];
        environmentFiles = [
          /var/lib/netbox/env/netbox.env
        ];
        volumes = [
          "/var/lib/netbox/startup_scripts:/opt/netbox/startup_scripts:z,ro"
          "/var/lib/netbox/initializers:/opt/netbox/initializers:z,ro"
          "/var/lib/netbox/configuration:/etc/netbox/config:z,rw"
          "/var/lib/netbox/reports:/opt/netbox/reports:z,ro"
          "/var/lib/netbox/scripts:/opt/netbox/scripts:z,ro"
          "/var/lib/netbox/media_files:/opt/netbox/media:z"
        ];
      };
      netbox-worker = {
        image = "netboxcommunity/netbox:v3.1";
        cmd = [
          "/opt/netbox/venv/bin/python"
          "/opt/netbox/netbox/manage.py"
          "rqworker"
        ];
        environmentFiles = [
          /var/lib/netbox/env/netbox.env
        ];
      };
      netbox-housekeeping = {
        image = "netboxcommunity/netbox:v3.1";
        cmd = [ "/opt/netbox/housekeeping.sh" ];
        environmentFiles = [
          /var/lib/netbox/env/netbox.env
        ];
      };
    };
  };

  networking = {
    hostName = "eos"; # After the Greek titan of dawn
    wireless.enable = false;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    defaultGateway = {
      address = "10.5.1.1";
      interface = "enp57s0u1";
    };
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
    interfaces.enp57s0u1 = {
      ipv4.addresses = [
        { address  = "10.5.1.10"; prefixLength = 24; }
      ];
    };
    firewall = {
      enable = false;
      allowedTCPPorts = [ 8123 ];
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # tailscale machine specific
  thecy.services.tailscale = let
    hosts = import ../../secrets/hosts.nix;
  in {
    enable = true;
    autoprovision = {
      enable = true;
      key = "${hosts.eos.tailscale.oneoffkey}";
    };
  };

  services.ddclient = {
    zone = "begyn.be";
    domains = [
      "dcf.begyn.be"
    ];
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  services.nginx.virtualHosts = {
    "prometheus.begyn.lan" = {
      serverAliases = [ "prometheus" ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:9090/";
      };
    };
    "alertmanager.begyn.lan" = {
      serverAliases = [ "alertmanager" ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:9093/";
      };
    };
    "grafana.begyn.lan" = {
      serverAliases = [ "grafana" ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:3000/";
      };
    };
    "netbox.begyn.be" = {
      forceSSL = true;
      serverName = "netbox.begyn.be";
      serverAliases = [
        "ipam.begyn.be"
      ];
      useACMEHost = "dcf.begyn.be";
      locations."/" = {
        proxyPass = "http://127.0.0.1:8080/";
      };
    };
    # "jupyterhub.dcf.begyn.be" = {
    #   forceSSL = true;
    #   serverName = "jupyterhub.dcf.begyn.be";
    #   useACMEHost = "dcf.begyn.be";
    #   locations."/" = {
    #     proxyPass = "http://127.0.0.1:8000/";
    #   };
    # };
  };

  services.postgresql = {
    ensureDatabases = [ "netbox" ];
    ensureUsers = [
      {
        name = "netbox";
        ensurePermissions = {
          "DATABASE netbox" = "ALL PRIVILEGES";
        };
      }
    ];
    enableTCPIP = true;
  };

  services.consul = {
    interface = {
      bind = "enp57s0u1";
    };
    extraConfig = {
      server = true;
      bootstrap_expect = 1;
      datacenter = "app-01";
      bind_addr = "10.5.1.10";
      client_addr = "10.5.1.10";
      enable_script_checks = true;
    };
  };

  services.redis = {
    bind = "0.0.0.0";
  };

  services.promtail = {
    enable = true;
    configuration = {
      server = {
        http_listen_port = 9080;
        grpc_listen_port = 0;
      };
      client.url = "http://localhost:3100/loki/api/v1/push";
      scrape_configs = [{
        job_name = "var-logs";
        static_configs = [
          {
            targets = [ "localhost" ];
            labels = {
              job = "varlogs";
              host = "eos";
              __path__ = "/var/log/*.log";
            };
          }
        ];
      }
      {
        job_name = "journald";
        journal = {
          max_age = "12h";
          labels = {
            job = "journald";
            host = "eos";
          };
        };
        relabel_configs = [{
          source_labels = [ "__journal__systemd_unit" ];
          target_label = "unit";
        }];
      }];
    };
  };

  services.loki = {
    enable = true;
    configuration = {
      auth_enabled = false;
      server.http_listen_port = 3100;
      ingester = {
        lifecycler = {
          address = "127.0.0.1";
          ring = {
            kvstore.store = "inmemory";
            replication_factor = 1;
          };
          final_sleep = "0s";
        };
        chunk_idle_period = "5m";
        chunk_retain_period = "30s";
      };
      schema_config.configs = [{
        from = "2020-05-15";
        store = "boltdb";
        object_store = "filesystem";
        schema = "v11";
        index = {
          prefix = "index_";
          period = "168h";
        };
      }];
      storage_config = {
        boltdb.directory = "/tmp/loki/index";
        filesystem.directory = "/tmp/loki/chunks";
      };
      limits_config = {
        enforce_metric_name = false;
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
      };
    };
  };

  services.prometheus= {
    ruleFiles = [
      ../../services/prometheus/rules/eos/node.rules
    ];
    scrapeConfigs = [
      {
        job_name = "node-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.1:9100"
              "eos:9100"
              "mail-01:9100"
              "hosting-01:9100"
              "unifi-01:9100"
            ];
        }];
      }
      {
        job_name = "website";
        scheme = "https";
        static_configs = [{
            targets = [
              "francis.begyn.be"
            ];
        }];
      }
      {
        job_name = "coredns";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.10:9153"
            ];
        }];
      }
      {
        job_name = "home-assistant";
        scheme = "http";
	metrics_path = "/api/prometheus";
	bearer_token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJhNWY2NzI5MjUxYWU0ZGUzYjAxYTY1YzdhZDM2NGQ0YSIsImlhdCI6MTY0NDY4NDUyMCwiZXhwIjoxOTYwMDQ0NTIwfQ.lexy4FxTVMVci5i7DicFRVe8Dgddwzhpf84T7COgxIg";
        static_configs = [{
            targets = [
              "10.5.1.10:8123"
            ];
        }];
      }
      {
        job_name = "tc-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.1:9704"
            ];
        }];
      }
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

