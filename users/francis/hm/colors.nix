{ config, pkgs, lib, ... }:

let
  hexColorType = lib.mkOptionType {
    name = "hex-color";
    descriptionClass = "noun";
    description = "RGB color in hex format";
    check = x: lib.isString x && !(lib.hasPrefix "#" x);
  };
in {
  options.colorScheme.palette = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.coercedTo lib.types.str (lib.removePrefix "#") hexColorType
    );
    description = ''
      Atttribute set describing predefined colors to be used in other places
      of the configuration.
    '';
  };

  config.colorScheme.palette = {
    base00 = "202020";
    base01 = "2a2827";
    base02 = "504945";
    base03 = "5a524c";
    base04 = "bdae93";
    base05 = "ddc7a1";
    base06 = "ebdbb2";
    base07 = "fbf1c7";
    base08 = "ea6962";
    base09 = "e78a4e";
    base0A = "d8a657";
    base0B = "a9b665";
    base0C = "89b482";
    base0D = "7daea3";
    base0E = "d3869b";
    base0F = "bd6f3e";
  };
}

