{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./acme.nix

    ../../../users
    ../../../users/francis
  ];

  networking = {
    hostName = "ingress-01";
    hostId = "aabbccdd";
    wireless.enable = false;
    useDHCP = false;
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
    firewall = {
      enable = true;
      allowedTCPPorts = [
        22 # ssh
        80 # HTTP
        443 # HTTPS
      ];
    };
  };
  services.resolved.extraConfig = ''
    DNSStubListener=no
  '';
  systemd.network= {
    enable = true;
    wait-online = {
      enable = true;
      ignoredInterfaces = [
        "tailscale*"
        "tailscale0"
        "veth*"
        "wlp*"
        "wlp3s0"
      ];
    };
    netdevs = {
      "120-lan" = {
        netdevConfig = { Kind = "vlan"; Name = "lan"; };
        vlanConfig.Id = 120;
      };
      "130-mgmt" = {
        netdevConfig = { Kind = "vlan"; Name = "mgmt"; };
        vlanConfig.Id = 130;
      };
      "190-lan" = {
        netdevConfig = { Kind = "vlan"; Name = "iot"; };
        vlanConfig.Id = 190;
      };
      "1100-lan" = {
        netdevConfig = { Kind = "vlan"; Name = "guests"; };
        vlanConfig.Id = 1100;
      };
    };
    networks = {
      "30-eth0" = {
        matchConfig.Name = "eth0";
        # address = [ "10.5.1.127/24" ];
        # routes = [ { Gateway = "10.5.1.5"; } ];
        vlan = [
          "lan"
          "mgmt"
          "iot"
          "guests"
        ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "carrier";
      };
      "130-mgmt" = {
        matchConfig.Name = "mgmt";
        address = [ "10.5.30.31/24" ];
        routes = [ { Gateway = "10.5.30.5"; } ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
      "10-tailscale0" = {
        matchConfig.Name = "tailscale*";
        linkConfig = {
	        Unmanaged = "yes";
          RequiredForOnline = "no";
        };
      };
    };
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  services.nginx = {
    enable = true;
  };

  # system packagesystem
  environment.systemPackages = with pkgs; [
    vim
    neovim
    wget
  ];

  users.users.root.password = "foobar";
  home-manager.users.francis = {
    imports = [
      ../../../users/francis/hm/go.nix
      ../../../users/francis/hm/configurations/fish.nix
      ../../../users/francis/hm/configurations/bash.nix
    ];
  };
  system.stateVersion = "25.05"; # Did you read the comment?
}
