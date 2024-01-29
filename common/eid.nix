{ pkgs, ...}:

let
  browser-eid-overlay = import ../overlays/browser-eid.nix;
in {
  # nixpkgs.overlays = [
  #   browser-eid-overlay
  # ];

  services.pcscd = {
    enable = true;
    plugins = [ pkgs.ccid ];
  };

  environment.etc."pkcs11/modules/opensc-pkcs11".text = ''
    module: ${pkgs.opensc}/lib/opensc-pkcs11.so
  '';

  # firefox
  programs = {
    firefox = {
      nativeMessagingHosts.packages = [ pkgs.web-eid-app ];
      policies.SecurityDevices.p11-kit-proxy = "${pkgs.p11-kit}/lib/p11-kit-proxy.so";
    };
  };
  # chrom(ium)
  environment.etc."chromium/native-messaging-hosts/eu.webeid.json".source = "${pkgs.web-eid-app}/share/web-eid/eu.webeid.json";
  environment.etc."opt/chrome/native-messaging-hosts/eu.webeid.json".source = "${pkgs.web-eid-app}/share/web-eid/eu.webeid.json";
}
