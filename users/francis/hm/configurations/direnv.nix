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
}
