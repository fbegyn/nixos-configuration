{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs.unstable; [
      kubectl
      kubectx
      stern
      kubernetes-helm
      k9s
      click
    ];
    programs.starship.settings.kubernetes.disabled = false;
  };
}
