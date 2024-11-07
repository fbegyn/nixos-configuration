{ pkgs, ... }:

{
  imports = [
    ../common
    ../users
  ];

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

  # services.fbegyn.website = {
  #   enable = false;
  #   useACMEHost = "begyn.be";
  #   domain = "francis.begyn.be";
  # };

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
  };
}

