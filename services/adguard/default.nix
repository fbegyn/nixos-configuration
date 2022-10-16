{ lib, ... }:

{
  services.adguardhome = {
    port = 3001;
    enable = true;
    openFirewall = true;
  };
}
