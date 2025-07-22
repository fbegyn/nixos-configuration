{ pkgs, ... }:
{
  services.gvfs.enable = true;
  services.gnome.sushi.enable = true;
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ dconf ];
  services.gnome.gnome-keyring.enable = true;

  programs = {
    wireshark = {
      enable = true;
      package = pkgs.unstable.wireshark;
    };
  };

  programs.projecteur = {
    enable = true;
    package = pkgs.unstable.projecteur;
  };

  environment.systemPackages = with pkgs; [
    firefox
    solaar
    logitech-udev-rules
    ltunify
  ];

  home-manager.users.francis = {
    imports = [ ./hm/gui.nix ];
  };
}
