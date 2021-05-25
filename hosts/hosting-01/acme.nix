{ pkgs, ... }:

let
  creds = pkgs.writeTextFile {
    name = "cf.env";
    text = builtins.readFile ../../secrets/cf.env;  
  };
  extraLegoFlags = [ "--dns.resolvers=8.8.8.8:53" ];
in {
  security.acme = {
    email = "francis+acme@begyn.be"; 
    acceptTerms = true;
  };

  security.acme.certs."francis.begyn.be" = {
    group = "nginx";
    email = "francis+hosting@begyn.be"; 
    dnsProvider = "cloudflare";
    credentialsFile = "${creds}";
    extraDomainNames = [ "*.francis.begyn.be" ];
    inherit extraLegoFlags;
  };
}
