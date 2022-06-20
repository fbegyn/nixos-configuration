{ ... }:

{
  programs.direnv = {
    enable = true;
    config = {
      global = {
        strict_env = true;
      };
    };
  };
  services.lorri.enable = true;
}
