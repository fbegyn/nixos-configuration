{ ... }:

{
  home-manager.users.francis = {
    services = {
      udiskie = {
        enable = true;
        automount = false;
        notify = true;
        tray = "auto";
      };
    };
  };
}
