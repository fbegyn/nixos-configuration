{ config, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    extraConfig = ''
      unload-module module-role-cork
      load-module module-raop-discover
    '';
    zeroconf.discovery.enable = true;
  };
}
