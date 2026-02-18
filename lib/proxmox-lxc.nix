{ config, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    (modulesPath + "/virtualisation/proxmox-lxc.nix")
  ];

  proxmoxLXC.enable = true;
  boot.isContainer = true;

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Brussels";

  # ZFS services don't work in containers
  systemd.services.zfs-mount.enable = false;
  systemd.services.zfs-share.enable = false;
  systemd.services.zfs-zed.enable = false;

  systemd.network.enable = true;
  systemd.network.wait-online = {
    enable = true;
    ignoredInterfaces = [
      "tailscale*"
      "tailscale0"
      "eth*"
      "wlp*"
      "wlp3s0"
    ];
  };

  services.openssh.enable = true;

  services.prometheus.exporters.node.enable = true;
  services.prometheus.exporters.node.enabledCollectors = [ "systemd" ];

  home-manager.users.francis = {
    imports = [
      ../users/francis/hm/go.nix
      ../users/francis/hm/configurations/fish.nix
      ../users/francis/hm/configurations/bash.nix
    ];
  };
}
