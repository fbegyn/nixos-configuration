{ pkgs, ...}:

{
  nixpkgs.overlays = [
    (self: super: {
      firefox = super.firefox.override { pkcs11Modules = [ self.eid-mw ];};
      firefox-bin = self.firefox;
    })
  ];

  home-manager.users.francis.home.packages = [
    pkgs.unstable.eid-mw
    pkgs.firefox
  ];

  services.pcscd = {
    enable = true;
    plugins = [ pkgs.ccid ];
  };
}
