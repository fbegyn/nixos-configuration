{ pkgs, ...}:

let
  browser-eid-overlay = import ../overlays/browser-eid.nix;
in {
  nixpkgs.overlays = [
    browser-eid-overlay
  ];

  environment.systemPackages = [
    pkgs.unstable.eid-mw
  ];

  services.pcscd = {
    enable = true;
    plugins = [ pkgs.unstable.ccid ];
  };
}
