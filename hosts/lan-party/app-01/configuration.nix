# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../../users
    ];

  # ZFS things
  boot.supportedFilesystems = [ "zfs" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "app-01"; # Define your hostname.
  networking.hostId = "abb01234";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    useDHCP = false;
    defaultGateway = "10.20.1.1";
    nameservers = [
      "10.20.1.1"
      "1.1.1.1"
      "8.8.8.8"
    ];
    interfaces = {
      eno3.useDHCP = false;
      eno4.useDHCP = false;
      enp1s0f0.useDHCP = false;
      enp1s0f1.useDHCP = false;
      bond0.ipv4.addresses = [{
        address = "10.30.10.1";
        prefixLength = 16;
      }];
      lan.ipv4.addresses = [{
        address = "10.20.10.1";
        prefixLength = 16;
      }];
      wifi.ipv4.addresses = [{
        address = "10.40.10.1";
        prefixLength = 16;
      }];
      iot.ipv4.addresses = [{
        address = "10.90.10.1";
        prefixLength = 16;
      }];
      pos.ipv4.addresses = [{
        address = "10.100.10.1";
        prefixLength = 16;
      }];
      security.ipv4.addresses = [{
        address = "10.110.10.1";
        prefixLength = 16;
      }];
    };
    bonds.bond0 = {
      interfaces = [ "eno1" "eno2" ];
      driverOptions = {
        mode = "802.3ad";
        lacp_rate = "slow";
      };
    };
    vlans = {
      lan = {
        id = 20;
        interface = "bond0";
      };
      wifi = {
        id = 40;
        interface = "bond0";
      };
      iot = {
        id = 90;
        interface = "bond0";
      };
      pos = {
        id = 100;
        interface = "bond0";
      };
      security = {
        id = 110;
        interface = "bond0";
      };
    };
    firewall = {
      interfaces = {
        bond0 = {
          allowedTCPPorts = [ 22 53 80 443 9090 3000 9100 3100 ];
          allowedUDPPorts = [ 53 ];
        };
        lan = {
          allowedTCPPorts = [ 53 80 443 ];
          allowedUDPPorts = [ 53 ];
        };
      };
    };
    localCommands = let
      interface = "lan";
      default_speed = "200Mbit";
      fallback_speed = "100Mbit";
    in ''
      tc qdisc add dev ${interface} root handle 1: hfsc default 9999
      tc class add dev ${interface} parent 1: classid 1:9999 hfsc sc rate ${fallback_speed} ul rate ${fallback_speed}
      tc class add dev ${interface} parent 1: classid 1:1 hfsc sc rate ${default_speed} ul rate ${default_speed}
      tc filter add dev ${interface} parent 1: u32 match ip dst 10.20.0.0/16 classid 1:1
    '';
  };

  environment.systemPackages = with pkgs; [
    vim
    screen
    unzip
    htop
    wget
    git
    jq
    ldns
    tcpdump
    dnsutils
    iftop
    conntrack-tools
    iotop
  ];

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };

  services.grafana = {
    enable = true;
    package = pkgs.unstable.grafana;
    settings.server = {
      http_addr = "10.30.10.1";
      http_port = 3000;
    };
  };

  services.prometheus = {
    enable = true;
    package = pkgs.unstable.prometheus;
    stateDir = "prometheus";
    scrapeConfigs = [
      {
        job_name = "prometheus";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.30.10.1:9090"
            ];
        }];
      }
      {
        job_name = "node-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.30.1.1:9100"
              "10.30.10.1:9100"
            ];
        }];
      }
      {
        job_name = "tc-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.30.1.1:9704"
            ];
        }];
      }
    ];
  };

  services.prometheus.exporters = {
    node = {
      enable = true;
    };
  };

  services.loki = {
    enable = true;
    dataDir = "/var/lib/loki";
    configFile = "/etc/loki/config.yaml";
  };

  # coredns
  services.coredns = let
    corednsconf = builtins.readFile /etc/coredns/config;
  in {
    enable = false;
    config = ''
      ${corednsconf}
    '';
  };
  # unbound
  systemd.services.unbound.serviceConfig.ReadWritePaths = [
    "/etc/nixos/unbound"
  ];
  services.unbound = {
    enable = true;
    package = pkgs.unbound-with-systemd;
    settings = {
      remote-control.control-enable = true;
      server = {
        interface = "0.0.0.0";
        access-control = [ "10.20.0.0/16 allow" "10.30.0.0/16 allow" "127.0.0.1/8 allow" ];
        verbosity = 1;
      };
      forward-zone = [
        { name = "."; forward-addr = [ "1.1.1.1" "8.8.8.8" "8.8.4.4" ]; }
      ];
      include = "/etc/nixos/unbound/*.conf";
    };
  };

  # lancache
  systemd.services.nginx.serviceConfig.ReadWritePaths = [
    "/var/log/lancache"
    "/var/lib/lancache"
    "/etc/nginx"
  ];
  services.nginx = let
    upstreamDns = "1.1.1.1";
    cacheDir = "/var/lib/lancache";
    logDir = "/var/log/lancache";
    cacheIndexSize = "500m";
    cacheMaxAge = "124d";
    cacheDiskSize = "1024000m";
    base = ''
      # Offer keepalive connections to clients that want to make use of them
      # Greatly improves efficiency across the board for larger deployments
      proxy_http_version 1.1;

      # Keep a client-side HTTP/1.1 alive until closed by the client
      # This is to work around buggy clients not expecting a "Connection: close"
      # after 100 requests and not bothering to re-initiate (Blizzard Web Client)
      keepalive_requests 1000000;

      # Upstream Configuration
      proxy_next_upstream error timeout http_404;
      proxy_redirect off;
      proxy_ignore_client_abort on;

      # Issue #14 - Blizzard downloads halting
      # This also impacts Origin, so we disable ETags across the board
      proxy_hide_header ETag;

      # Upstream request headers
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

      # Debug Headers
      add_header X-Upstream-Status $upstream_status;
      add_header X-Upstream-Response-Time $upstream_response_time;
      add_header X-Upstream-Cache-Status $upstream_cache_status;
    '';
    cache = ''
      # Make sure to include this config in higher-level configuration!
      # eg. proxy_cache installs

      # Only download one copy at a time and use a large timeout so
      # this really happens, otherwise we end up wasting bandwith
      # getting the file multiple times.
      proxy_cache_lock on;
      proxy_cache_lock_timeout 1h;

      # Allow the use of state entries
      proxy_cache_use_stale error timeout invalid_header updating http_500 http_502 http_503 http_504;

      # Allow caching of 200 but not 301 or 302 as our cache key may not include query params
      # hence may not be valid for all users
      proxy_cache_valid 200 206 270d;
      proxy_cache_valid 301 302 0;

      # Enable cache revalidation
      proxy_cache_revalidate on;

      # Don't cache requests marked as nocache=1
      proxy_cache_bypass $arg_nocache;

      # 40G max file
      proxy_max_temp_file_size 40960m;
    '';
    pass = ''
      # Proxy the request to the original host and URI
      proxy_pass http://$host$request_uri;
    '';
    noslice = ''
      # Cache Settings
      proxy_cache_key   $uri;
    '';
    slice = ''
      # Request slicing, supported since nginx 1.9.8
      slice 1m;

      # Cache Settings
      proxy_cache_key   $uri$slice_range;
      proxy_set_header  Range $slice_range;
    '';
  in {
    enable = true;
    # the slice module is not build by default, compile it for our purposes
    package = pkgs.nginxMainline.overrideAttrs (oldAttrs: {
      configureFlags = oldAttrs.configureFlags ++ ["--with-http_slice_module"];
    });
    config = ''
      # Main Nginx Configuration, use sites-enabled/conf.d to install apps
      worker_processes auto;

      # Increase the open file limit for workers
      worker_rlimit_nofile 100000;

      events {
        worker_connections 4096;
        multi_accept on;
        use epoll;
      }

      http {
        # General/Transport
        sendfile on;
        tcp_nopush on;
        tcp_nodelay on;

        # Disabled proxy_buffering to avoid temporary files. This starts streaming the response immediately instead of caching it in memory first
        # (instead of writing to disk in case proxy_buffer_size is exceeded).
        # EXPIRMENTAL DISABLED FOR NOW
        #proxy_buffering off;
        #proxy_buffer_size 128k;

        # Disable dynamic gzip compression as this doesn't work in combination with sendfile.
        gzip off;

        proxy_headers_hash_bucket_size 128;
        types_hash_max_size 2048;

        # Enable thread pools (requires nginx built with --with-file-aio)
        aio threads;
        aio_write on;
        directio 4m;

        include /etc/nginx/mime.types;
        default_type application/octet-stream;

        # Logging
        access_log /var/log/nginx/access.log;
        error_log /var/log/nginx/error.log;

        # Ubiquitous Log Format
        log_format depot
                  '[$time_local] '
                  '$remote_addr '
                  '$request_method '
                  '"$request_uri" '
                  '$http_range '
                  '$slice_range '
                  '$status '
                  '$body_bytes_sent '
                  '$upstream_response_length '
                  '$upstream_cache_status '
                  '$host '
                  '$upstream_status '
                  '$upstream_response_time '
                  '"$http_user_agent"';

        # Fix for httpoxy (CVE-2016-5385)
        fastcgi_param HTTP_PROXY "";

        # map for cache identification
        map "$http_user_agent£££$http_host" $cacheidentifier {
          default $http_host;
          ~Valve\/Steam\ HTTP\ Client\ 1\.0£££.* steam;
        }

        # Includes
        include /etc/nginx/conf.d/*.conf;
        include /etc/nginx/sites-enabled/*;
      }
      stream {
        log_format stream_basic '$remote_addr [$time_local] $protocol $status $ssl_preread_server_name $bytes_sent $bytes_received $session_time';
        server {
          listen 443;
          resolver 1.1.1.1 ipv6=off;
          proxy_pass  $ssl_preread_server_name:443;
          ssl_preread on;
          access_log /var/log/lancache/stream-access.log stream_basic;
          error_log /var/log/lancache/stream-error.log;
        }
      }
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
