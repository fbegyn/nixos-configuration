{ lib, ... }:

{
  services.redis = {
    enable = true;
    openFirewall = true;
    requirePassFile = "/var/lib/redis/pass";
  };
}
