{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.gns3-gui
    unstable.gns3-server
  ];
}
