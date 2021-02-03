{ config, pkgs, ... }:

{
  # make steam work
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages32 = with pkgs.pkgsi686Linux; [ libva  ];
  };
  hardware.pulseaudio.support32Bit = true;
  home-manager.users.francis = {
    home.packages = with pkgs; [
      steam
    ];
  };
}

