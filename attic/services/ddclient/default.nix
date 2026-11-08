{ config, ... }:

{
  services.ddclient = {
    interval = "10m";
    passwordFile = "/etc/ddclient.key";
  };
}
