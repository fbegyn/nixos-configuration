self: super: {
  firefox = super.firefox.override { pkcs11Modules = [ self.eid-mw ];};
  firefox-bin = self.firefox;
}
