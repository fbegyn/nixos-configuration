
{ config, pkgs, ... }:

{
  fonts.fontDir.enable = true;
  fonts = {
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      liberation_ttf
      fira-code-symbols
      freefont_ttf
      montserrat
      dina-font
      symbola
      (nerdfonts.override { fonts = [
        "FiraCode"
        "DejaVuSansMono"
      ];})
    ];
    fontconfig = {
      enable = true;
    };
  };
}
