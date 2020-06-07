{pkgs, ...}:

{
  home-manager.users.francis = {
    programs.fish = {
      enable = true;
    };
  };
}
