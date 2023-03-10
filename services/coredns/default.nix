{ lib, ... }:

let
  corednsconf = builtins.readFile ./config;
  blocklist = builtins.readFile (builtins.fetchurl {
    url =
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews/hosts";
    sha256 = "sha256:1hxhsbcpj205gzrivrslxza1bp34a3kx8gcd746f8bb7zdiqlyxq";
  });

  dnsSOA = domain: primary: ipv4: email: ''
    @	3600	SOA	${primary}.${domain}.   ${email}.${domain}. (
                1       ; serial
                3600    ; refresh
                600     ; retry
                604800  ; expire
                1800	; Negative resp cache TTL
    )

    ; nameserver dns
    86400                     NS     ${primary}.${domain}
    ${primary}        IN      A      ${ipv4}
  '';
  hostv4 = name: ipv4: ''
    ${name}               IN      A       ${ipv4}
    ${name}.ipv4          IN      A       ${ipv4}'';
  cname = name: alias: ''${name}               IN      CNAME   ${alias}'';
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
        ${dnsSOA "begyn.lan" "ns1" "10.5.1.10" "admin"}

        ; A records
        ${hostv4 "router" "10.5.1.1"}
        ${hostv4 "eos" "10.5.1.10"}

        ; CNAME records
        ${cname "prometheus" "eos"}
        ${cname "alertmanager" "eos"}
        ${cname "grafana" "eos"}
        ${cname "loki" "eos"}
        ${cname "plex" "eos"}
      '';
    };
    coredns-ads-fakenews = {
      enable = true;
      target = "coredns/hosts/ads-fakenews";
      text = ''
        ${blocklist}
      '';
    };
  };
}
