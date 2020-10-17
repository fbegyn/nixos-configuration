{ pkgs, config, ...  }:

{
  home-manager.users.francis = {
    home.packages = [
      pkgs.unstable.spotify-tui
    ];
    services.spotifyd = {
      enable = true;
      settings = {
        global = {
          username = "francis.begyn@gmail.com";
          password_cmd = "/home/francis/.nix-profile/bin/pass entertainment/spotify";
          device_name = "spotifyd";
          device_type = "computer";
          cache_path = "/home/francis/.cache/spotify/Storage";
          no_audio_cache = "false";
          bitrate = "320";
          volume_normalisation = "true";
          normalisation_pregain = "-10";
          backend = "pulseaudio";
          mixer = "PCM";
          volume_controller = "softvol";
          zeroconf_port = "1234";
        };
      };
    };
  };
}
