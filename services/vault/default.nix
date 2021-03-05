{ config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    unstable.vault-bin
  ];
  services.vault = {
    package = pkgs.unstable.vault-bin;
    enable = true;
    address = "10.5.1.10:8200";
    storageBackend = "file";
    extraConfig = ''
      ui = true
    '';
  };
}
