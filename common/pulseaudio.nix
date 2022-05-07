{ config, ... }:

{
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = false;
    extraConfig = ''
      unload-module module-role-cork
    '';
  };
}
