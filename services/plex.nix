{ pkgs, ... }:

{
  containers = {
    plex = {
      autoStart = true;
      bindMounts = {
        "/media/videos" = {
          hostPath = "/home/francis/Videos";
          isReadOnly = true;
        };
      };
      config = { ... }: {
          services.plex = {
            enable = true;
            package = pkgs.unstable.plex;
          };
        };
    };
  };
}
