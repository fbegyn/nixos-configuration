{ config, ... }:

{
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraConfig = ''
      unload-module module-role-cork
    '';
  };
}
