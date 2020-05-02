{ config, pkgs, ... }:

{
  home = {
    file.".config/nvim/init.vim".source = ./init.vim;
  };

  programs.neovim = {
    enable =  true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true;
  };
}
