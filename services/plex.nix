{ pkgs, ... }:

let
in {
  containers = {
    plex = {
      autoStart = true;
      bindMounts = {
        "/media/videos" = {
          hostPath = "/home/francis/Videos";
          isReadOnly = true;
        };
      };
      config = { ... }:
        let
          unstable =
            import <nixos-unstable-small> { config.allowUnfree = true; };
        in {
          services.plex = {
            enable = true;
            package = unstable.plex;
          };
        };
    };
  };
}
