{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.tailscale
  ];

  services.tailscale = {
    enable = true;
    package = pkgs.unstable.tailscale;
  };
}
