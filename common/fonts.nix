
{ config, pkgs, ... }:

{
  fonts.fontDir.enable = true;
  fonts = {
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      liberation_ttf
      fira-code-symbols
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
