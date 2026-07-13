{ pkgs, ... }:
# VSCode config mirroring the emacs (evil) and nvim setups in sibling configs.
# Leader = Space, Gruvbox dark hard, same b/c/f/g/p/q/s/t/w prefix map.
#
# Extensions are NOT declared in Nix (several — VSpaceCode, gruvbox, copilot —
# are not in nixpkgs); install via the Marketplace on first run.
# Required set (leader menu + modal editing):
#   asvetliakov.vscode-neovim          (vim layer, drives modal bindings)
#   VSpaceCode.vspacecode              (spacemacs-style leader menu — SPC popup)
#   VSpaceCode.whichkey                (which-key hints; backs vspacecode)
#   jdinhlife.gruvbox                  (theme; settings expect "Gruvbox Dark Hard")
#   vscode-icons-team.vscode-icons     (icon theme)
#   golang.go
#   rust-lang.rust-analyzer
#   jnoortheen.nix-ide                 (uses nil)
#   elixir-lsp.vscode-elixir-ls
#   ms-python.python
#   ms-python.vscode-pylance
#   charliermarsh.ruff
#   denoland.vscode-deno
#   sumneko.lua
#   mads-hartmann.bash-ide-vscode
#   redhat.vscode-yaml
#   ms-azuretools.vscode-docker
#   eamodio.gitlens                    (git blame, hunk nav)
#   mhutchie.git-graph                 (git log view)
#   ms-vscode-remote.remote-ssh        (TRAMP analog)
#   github.copilot                     (optional)
{
  programs.vscode = {
    enable = true;
    package = pkgs.unstable.vscode;
    mutableExtensionsDir = true;

    profiles.default = {
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = true;
      userSettings = import ./settings.nix;
      keybindings = import ./keybindings.nix;
    };
  };
}
