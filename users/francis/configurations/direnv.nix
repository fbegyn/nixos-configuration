{ ... }:

{
  home-manager.users.francis = {
    programs.direnv = {
      enable = true;
      enableFishIntegration = true;
      config = {
        global = {
          strict_env = true;
        };
      };
    };
    services.lorri.enable = true;
  };
}
