{ config, pkgs, ... }: {
  security.rtkit.enable = true;
  sounds.enable = true;

  environment.systemPackages = with pkgs.unstable; [
    pavucontrol
  ];

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # config.pipewire.context = {
    #   modules = [{ name = "libpipewire-module-raop-discover"; }];
    #   properties = {
    #     "link.max-buffers" = 16; # version < 3 clients can't handle more than this
    #     "log.level" = 2; # https://docs.pipewire.org/page_daemon.html
    #     #"default.clock.rate" = 48000;
    #     #"default.clock.quantum" = 1024;
    #     #"default.clock.min-quantum" = 32;
    #     #"default.clock.max-quantum" = 8192;
    #   };
    # };
  };

  hardware.pulseaudio = {
    enable = false;
    extraConfig = ''
      unload-module module-role-cork
      load-module module-raop-discover
    '';
    zeroconf.discovery.enable = true;
  };
}
