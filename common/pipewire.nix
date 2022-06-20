{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pavucontrol
  ];
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    config.pipewire = {
      "context.properties" = {
        "link.max-buffers" = 16; # version < 3 clients can't handle more than this
        "log.level" = 2; # https://docs.pipewire.org/page_daemon.html
        #"default.clock.rate" = 48000;
        #"default.clock.quantum" = 1024;
        #"default.clock.min-quantum" = 32;
        #"default.clock.max-quantum" = 8192;
      };
      "context.modules" = [
        { name = "libpipewire-raop-discover"; args = { }; }
      ];
    };
  };
}
