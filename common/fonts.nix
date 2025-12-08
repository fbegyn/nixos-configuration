
{ config, pkgs, ... }:

{
  fonts.fontDir.enable = true;
  fonts = {
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      liberation_ttf
      fira-code-symbols
      freefont_ttf
      montserrat
      dina-font
      symbola
      nerd-fonts.fira-code
      nerd-fonts.dejavu-sans-mono
    ];
    fontconfig = {
      enable = true;
    };
  };
}
