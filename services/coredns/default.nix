{ lib, ... }:

let
  corednsconf = builtins.readFile ./config;
  host = builtins.readFile ./hosts/ads-fakenews;
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
    ${name}.ipv4          IN      A       ${ipv4}
  '';
  cname = name: alias: ''
    ${name}               IN      CNAME   ${alias}
  '';
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

        ${hostv4 "router" "10.5.1.1"}
        ${hostv4 "eos" "10.5.1.10"}

        ${cname "unifi" "eos"}
        ${cname "consul" "eos"}
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
