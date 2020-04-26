{ config, pkgs, ... }:

{
  vim_configurable.customize {
  name = "vim-with-plugins";
  vimrcConfig.plug.plugins = with pkgs.vimPlugins; [vim-addon-nix youcompleteme];
};
