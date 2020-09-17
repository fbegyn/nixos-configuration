{ pkgs, ...}:

{
  imports = [
    ../../secrets/fish.nix
  ];
  home-manager.users.francis = {
    home.packages = [
      pkgs.unstable.starship
    ];
    xdg = {
      configFile = {
        "fish/functions/humanize_duration.fish" = {
          text = (builtins.fetchurl "https://raw.githubusercontent.com/fishpkg/fish-humanize-duration/master/humanize_duration.fish");
        };
      };
    };
    programs.fish = {
      enable = true;
      package = pkgs.unstable.fish;
      promptInit = ''
        starship init fish | source
      '';
      shellAliases = {
        "gst" = "git status";
        "ga" = "git add";
        "glg" = "git log";
        "gc" = "git commit";
        "gcmsg" = "git commit -m";
      };
      loginShellInit = ''
        set -Ux SSH_AUTH_SOCK /run/user/1000/ssh-agent
        set -Ux BROWSER qutebrowser
        set -Ux EDITOR nvim
        set -Ua fish_user_paths ~/.local/bin
        set -Ua fish_user_paths ~/.cargo/bin
        set -Ua fish_user_paths ~/go/bin
      '';
      plugins = [
        {
          name = "z";
          src = pkgs.fetchFromGitHub {
            owner = "jethrokuan";
            repo = "z";
            rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
            sha256 = "0c5i7sdrsp0q3vbziqzdyqn4fmp235ax4mn4zslrswvn8g3fvdyh";
          };
        }
        #{
        #  name = "done";
        #  src = pkgs.fetchFromGitHub {
        #    owner = "franciscolourenco";
        #    repo = "done";
        #    rev = "cad91110ed69b3ba6a85771f1f91df5af5c9ddd6";
        #    sha256 = "162pzvp40ij9hyp197w7fshq94rlxd1jpi8wnhmzqv6i91g2p9k3";
        #  };
        #}
        {
          name = "fzf";
          src = pkgs.fetchFromGitHub {
            owner = "jethrokuan";
            repo = "fzf";
            rev = "24f4739fc1dffafcc0da3ccfbbd14d9c7d31827a";
            sha256 = "0kz057nr07ybh0y06ww3p424rgk8pi84pnch9jzb040qqn9a8823";
          };
        }
      ];
    };
  };
}
