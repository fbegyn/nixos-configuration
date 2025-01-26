# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./acme.nix
    ../../common
    ../../users
    ../../services/tailscale.nix
  ];

  networking.interfaces.ens10.useDHCP = true;
  networking.interfaces.ens3.useDHCP = true;
  networking.defaultGateway6 = {
    address = "fe80::1";
    interface = "ens3";
  };
  networking.interfaces.ens3.ipv6 = {
    addresses = [
      {
        address = "2a01:4f9:c010:d553::1";
        prefixLength = 64;
      }
    ];
  };
  networking.firewall.package = pkgs.iptables-nftables-compat;
  networking.firewall.interfaces = {
    "tailscale0" = {
      allowedTCPPorts = [ 22 9100 53 ];
      allowedUDPPorts = [ 53 ];
    };
    "podman+" = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };
  };
  networking.firewall = {
    allowedTCPPorts = [
      80 443
      8080  # Port for UAP to inform controller.
      8880  # Port for HTTP portal redirect, if guest portal is enabled.
      8843  # Port for HTTPS portal redirect, ditto.
      6789  # Port for UniFi mobile speed test.
    ];
    allowedUDPPorts = [
      3478  # UDP port used for STUN.
      10001 # UDP port used for device discovery.
    ];
  };

  # podman config
  virtualisation.podman = {
    enable = true;
    dockerSocket.enable = true;
    defaultNetwork.settings = {
      dns_enabled = true;
    };
  };

  services.prometheus.exporters.node.enable = true;

  services.fbegyn.website = {
    enable = true;
    useACMEHost = "begyn.be";
    domain = "francis.begyn.be";
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  # unifi
  virtualisation.oci-containers = {
    backend = "podman";
    containers = {
      "mongodb" = {
        image = "mongo:5.0.22";
        volumes = [
          "mongodb-data:/data/db"
          "/home/francis/unifi-controller/init-mongo.js:/docker-entrypoint-initdb.d/init-mongo.js:ro"
        ];
        cmd = [ "--auth" ];
      };
      "unifi-controller" = {
        image = "linuxserver/unifi-network-application:8.0.7";
        ports = [
          "127.0.0.1:8443:8443"
          "8880:8880"
          "8843:8843"
          # Open parts directly through DNAT firewalling
          "8080:8080"
          "3478:3478/udp"
          "10001:10001/udp"
          "6789:6789"
          "5514:5514/udp"
        ];
        volumes = [ "/home/francis/unifi-controller/config:/config" ];
      };
    };
  };
  services.nginx.virtualHosts = {
    "unifi.svc.begyn.be" = {
      forceSSL = true;
      useACMEHost = "svc-01.begyn.be";
      locations = {
        "/" = {
          proxyPass = "https://127.0.0.1:8443$request_uri";
          extraConfig = ''
            client_max_body_size 15M;
            proxy_ssl_verify off;
            proxy_ssl_session_reuse on;
            proxy_buffering off;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
          '';
        };
        "/wss/" = {
          proxyPass = "https://127.0.0.1:8443";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_buffering off;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
            proxy_read_timeout 86400;
          '';
        };
      };
    };
  };

  # weechat
  environment.systemPackages = [ pkgs.weechat ];
  systemd.services.weechat = {
    description = "weechat headless";
    serviceConfig = {
      User = "francis";
      Group = "francis";
      ExecStart = [ "${pkgs.weechat}/bin/weechat-headless -d /var/lib/weechat --stdout" ];
      WorkingDirectory = "/var/lib/weechat";
    };
    wantedBy = [ "default.target" ];
  };
  services.nginx.commonHttpConfig = ''
    limit_req_zone $binary_remote_addr zone=weechat:10m rate=5r/m;
  '';
  services.nginx.virtualHosts = {
    "irc.francis.begyn.be" = {
      forceSSL = true;
      useACMEHost = "begyn.be";
      locations."^~ /weechat" = {
        proxyPass = "http://100.93.146.4:9001";
        proxyWebsockets = true;
	extraConfig = ''
	  proxy_read_timeout 4h;
	  limit_req zone=weechat burst=1 nodelay;
	'';
      };
      locations."/".root = pkgs.glowing-bear;
    };
  };

  services.nginx.virtualHosts = {
    "oauth2.svc-01.begyn.be" = {
      forceSSL = true;
      useACMEHost = "svc-01.begyn.be";
    };
  };
  services.oauth2-proxy = let
    hosts = import ../../secrets/hosts.nix;
  in {
    enable = true;
    email.addresses = ''
      francis.begyn@gmail.com
    '';
    extraConfig = {
      whitelist-domain = ".begyn.be";
      cookie-csrf-expire = "6h";
      cookie-csrf-per-request = true;
    };
    nginx = {
      domain = "oauth2.svc-01.begyn.be";
      virtualHosts = {
        "irc.francis.begyn.be" = {
          allowed_emails = [ "francis.begyn@gmail.com" ];
        };
      };
    };
    google = {
      serviceAccountJSON =
        "${hosts.hosting-01.oauth2_proxy.google.serviceAccountJSON}";
    };
    clientID = "${hosts.hosting-01.oauth2_proxy.clientID}";
    keyFile = "${hosts.hosting-01.oauth2_proxy.keyFile}";
    cookie = {
      domain = ".begyn.be";
      secret = "${hosts.hosting-01.oauth2_proxy.cookie.secret}";
      expire = "12h0m0s";
      refresh = "11h30m0s";
    };
  };

  # tailscale machine specific
  services.fbegyn.tailscale = let
    hosts = import ../../secrets/hosts.nix;
  in {
    enable = false;
    autoprovision = {
      enable = true;
      key = "${hosts.tailscale.tempkey}";
      options = [ "--advertise-tags=tag:prod,tag:hetzner,tag:cloud" ];
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = false;
  };

  # roundcube webmail
  services.postgresql.package = pkgs.postgresql_15;
  services.roundcube = {
    enable = true;
    package = pkgs.roundcube;
    dicts = with pkgs.aspellDicts; [en nl fr de];
    hostName = "webmail.begyn.be";
    extraConfig = ''
      $config['des_key'] = 'TOEADOJDzA1CikSrPIOBVodn25fsYElV';

      $config['default_host'] = 'tls://mail.begyn.be';
      $config['smtp_server'] = 'tls://mail.begyn.be';
      $config['smtp_port'] = 587;

      // For STARTTLS IMAP
      $config['imap_conn_options'] = array(
          'ssl' => array(
            'verify_peer'       => true,
            // certificate is not self-signed if cafile provided
            'allow_self_signed' => false,
            // For Letsencrypt use the following two lines and remove the 'cafile' option above.
            //'ssl_cert' => '/etc/letsencrypt/live/mail.my_domain.org/fullchain.pem',
            //'ssl_key'  => '/etc/letsencrypt/live/mail.my_domain.org/privkey.pem',
            // probably optional parameters
            'ciphers' => 'TLSv1+HIGH:!aNull:@STRENGTH',
            'peer_name'         => 'mail.begyn.be',
          ),
      );
      // For STARTTLS SMTP
      $config['smtp_conn_options'] = array(
          'ssl' => array(
            'verify_peer'       => true,
            // certificate is not self-signed if cafile provided
            'allow_self_signed' => false,
            // For Letsencrypt use the following two lines and remove the 'cafile' option above.
            //'ssl_cert' => '/etc/letsencrypt/live/mail.my_domain.org/fullchain.pem',
            //'ssl_key'  => '/etc/letsencrypt/live/mail.my_domain.org/privkey.pem',
            // probably optional parameters
            'ciphers' => 'TLSv1+HIGH:!aNull:@STRENGTH',
            'peer_name'         => 'mail.begyn.be',
          ),
      );
    '';
  };
  services.nginx.virtualHosts = {
    "webmail.begyn.be" = {
      enableACME = false;
      forceSSL = true;
      useACMEHost = "begyn.be";
    };
  };
}

