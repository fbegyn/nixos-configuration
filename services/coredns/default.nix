{ ... }:

let
  corednsconf = builtins.readFile ./config;
  begyn-zone = builtins.readFile ./zones/db.begyn.lan;
  host = builtins.readFile ./hosts/ads-fakenews;
in
{
  services.coredns = {
    enable = true;
    config = ''
      ${corednsconf}
    '';
  };

  environment.etc = {
    coredns-db-begyn = {
      enable = true;
      target = "coredns/zones/db.begyn.lan";
      text = ''
        ${begyn-zone}
      '';
    };
    coredns-ads-fakenews = {
      enable = true;
      target = "coredns/hosts/ads-fakenews";
      text = ''
        ${host}
      '';
    };
  };
}
