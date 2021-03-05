{ config, pkgs, ...}:

{
  services.consul = {
    package = pkgs.unstable.consul;
    enable = true;
    webUi = true;
    interface = {
      bind = "enp57s0u1";
    };
    extraConfig = {
      server = true;
      bootstrap_expect = 1;
      datacenter = "app-01";
      bind_addr = "10.5.1.10";
      client_addr = "10.5.1.10";
      enable_script_checks = true;
    };
  };
}
