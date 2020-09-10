
{ config, pkgs, ... }:

{
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
      dina-font
      symbola
      nerdfonts
    ];
    fontconfig = {
      enable = true;
    };
  };
}
