{ config, ... }:

{
  security.acme = {
    defaults.email = "francis+acme@begyn.be";
    defaults.dnsResolver = "1.1.1.1:53";
    acceptTerms = true;
  };

  security.acme.certs."ingress-01.dcf.begyn.be" = {
    group = "nginx";
    email = "francis+hosting@begyn.be";
    dnsProvider = "cloudflare";
    credentialsFile = config.age.secrets."secrets/api/cf".path;
    extraDomainNames = [];
  };
}
