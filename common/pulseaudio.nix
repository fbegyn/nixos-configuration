{ config, ... }:

{
  hardware.pulseaudio = {
    enable = false;
    extraConfig = ''
      unload-module module-role-cork
    '';
  };
}
