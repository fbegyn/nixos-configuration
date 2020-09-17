{ ... }:

let
  corednsconf = builtins.readFile ./config
in
{
  services.coredns = {
    enable = true;
    config = ''
      ${corednsconf}
    '';
  };
}
