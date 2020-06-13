{pkgs, ...}:

{
  home-manager.users.francis = {
    programs.fish = {
      enable = true;
      shellAliases = {
        "gst" = "git status";
        "ga" = "git add";
        "glg" = "git log";
        "gc" = "git commit";
        "gcmsg" = "git commit -m";
      };
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
        #  name = "pure";
        #  src = pkgs.fetchFromGitHub {
        #    owner = "rafaelrinaldi";
        #    repo = "pure";
        #    rev = "d66aa7f0fec5555144d29faec34a4e7eff7af32b";
        #    sha256 =              "0klcwlgsn6nr711syshrdqgjy8yd3m9kxakfzv94jvcnayl0h62w";
        #  };
        #}
        {
          name = "done";
          src = pkgs.fetchFromGitHub {
            owner = "franciscolourenco";
            repo = "done";
            rev = "cad91110ed69b3ba6a85771f1f91df5af5c9ddd6";
            sha256 = "162pzvp40ij9hyp197w7fshq94rlxd1jpi8wnhmzqv6i91g2p9k3";
          };
        }
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
