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
}
