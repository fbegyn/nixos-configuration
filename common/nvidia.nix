{ config, ... }:

{
  # nvidia drivers
  services.xserver.videoDrivers = [ "nvidia" ];
}
