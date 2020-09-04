{ pkgs, ... }:

let
  notesDir = "/home/francis/zettelkasten";
  neuron = (
    let neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/master.tar.gz";
    in import neuronSrc {});
in
{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      neuron
    ];
    systemd.user.services.neuron = {
      Unit.Description = "Neuron Zettelkasten service";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${neuron}/bin/neuron -d ${notesDir} rib -ws 127.0.0.1:15860";
      };
    };
  };
}
