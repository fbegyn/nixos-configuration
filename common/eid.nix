{ pkgs, ...}:

let
  browser-eid-overlay = import ../overlays/browser-eid.nix;
in {
  nixpkgs.overlays = [
    browser-eid-overlay
  ];

  home-manager.users.francis.home.packages = [
    pkgs.unstable.eid-mw
  ];

  services.pcscd = {
    enable = true;
    plugins = [ pkgs.ccid ];
  };
}
