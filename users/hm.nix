{ pkgs, home-manager, ... }:

{
  home-manager.users = {
    francis = (import ./francis/home.nix);
  };
}
