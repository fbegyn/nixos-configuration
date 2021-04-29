{
  systemd.services.website = {
    enable = true;
    unitConfig = {
      description = "This hosts francis.begyn.be";
    };
    serviceConfig = {
      User = "francis"; 
      Group = "francis";
      WorkingDirectory = "/home/francis/francis.begyn.be";
      ExecStart = "/home/francis/Go/bin/websiteserver";
    };
    wantedBy = [ "default.target" ];
    after = [ "network.target" ];
  };

  services.nginx.enable = true;
  # francis.begyn.be website/blog
  services.nginx.virtualHosts."francis.begyn.be" = {
    forceSSL = true;
    enableACME = true;
    root = "/var/www/francis.begyn.be";
    locations."/" = {
      proxyPass = "http://localhost:3114";
      extraConfig = ''
        add_header Permissions-Policy interest-cohort=();
      '';
    };
  };
  security.acme.certs."francis.begyn.be".email = "francis.begyn+website@gmail.com";

  # francis.begyn.eu website/blog - redirect
  services.nginx.virtualHosts."francis.begyn.eu" = {
    forceSSL = true;
    enableACME = true;
    root = "/var/www/francis.begyn.eu";
    locations."/" = {
      proxyPass = "http://localhost:3114";
      extraConfig = ''
        add_header Permissions-Policy interest-cohort=();
      '';
    };
  };
  security.acme.certs."francis.begyn.eu".email = "francis.begyn+website@gmail.com";

  networking.firewall = {
    allowedTCPPorts = [
      80
      443
    ];
  };
}
