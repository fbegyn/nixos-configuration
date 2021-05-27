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
            import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-unstable-small.tar.gz) {
              config.allowUnfree = true;
            };
        in {
          services.plex = {
            enable = true;
            package = unstable.plex;
          };
        };
    };
  };
}
