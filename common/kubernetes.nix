{ pkgs, ... }:

{
  home-manager.users.francis.home.packages = with pkgs; [
    unstable.kubectl
    unstable.kubectx
    unstable.stern
    unstable.kubernetes-helm
    unstable.k9s
  ];
}
