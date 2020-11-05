{ config, pkgs, ...}:

{
  services.consul = {
    package = pkgs.unstable.consul;
    enable = true;
    webUi = true;
    interface = {
      bind = "eno1";
    };
    extraConfig = {
      server = true;
      bootstrap_expect = 1;
      datacenter = "ouders-home-01";
      bind_addr = "10.3.10.10";
      client_addr = "10.3.10.10";
      enable_script_checks = true;
    };
  };
}
