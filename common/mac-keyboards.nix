{ config, pkgs, ... }:

{
  boot.extraModprobeConfig = ''
    options hid_apple swap_opt_cmd=1
  '';
}
