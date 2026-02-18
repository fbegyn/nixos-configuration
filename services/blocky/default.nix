{ config, lib, ... }:

{
  services.blocky = {
    enable = true;
    settings = {
      ports.dns = 53;
      ports.tls = 853;
      ports.http = 14000;
      log.format = "json";
      upstreams.groups = {
        default = lib.mkDefault [
          "https://one.one.one.one/dns-query"
          "1.1.1.1"
          "8.8.8.8"
        ];
      };
      bootstrapDns = [
        { upstream = "https://one.one.one.one/dns-query"; ips = [ "1.1.1.1" "8.8.8.8" ]; }
      ];
      blocking = {
        blackLists = {
          ads = [
            "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt"
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
          ];
        };
        clientGroupsBlock = {
          default = [
            "ads"
          ];
        };
      };
      caching = {
        minTime = "4h";
        maxTime = "48h";
        maxItemsCount = 5000;
        prefetching = true;
        prefetchMaxItemsCount = 300;
      };
      prometheus.enable = true;
      queryLog = {
        type = "csv";
        target = "/var/tmp";
        logRetentionDays = 5;
        fields = [ "question" "duration" "responseReason" "responseAnswer" ];
        flushInterval = "30s";
      };
    };
  };
}
