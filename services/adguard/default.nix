{ lib, ... }:

let
  blocklist = builtins.readFile (builtins.fetchurl "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews/hosts");
in
{
  services.adguardhome = {
    port = 3001;
    enable = true;
    openFirewall = true;
  };
}
