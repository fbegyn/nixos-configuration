{ config, pkgs, ... }:

{
  programs.neovim = {
    enable =  true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true;
  };
}
