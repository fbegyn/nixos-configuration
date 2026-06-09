{ pkgs, ... }:
# VSCode config mirroring the emacs (evil) and nvim setups in sibling configs.
# Leader = Space, Gruvbox dark hard, same b/c/f/g/p/q/s/t/w prefix map.
#
# Extensions are NOT declared in Nix — install via the Marketplace on first run.
# Recommended set:
#   asvetliakov.vscode-neovim          (vim layer, drives modal bindings)
#   jdinhlife.gruvbox                  (theme; settings expect "Gruvbox Dark Hard")
#   vscode-icons-team.vscode-icons     (icon theme)
#   VSpaceCode.vspacecode              (spacemacs-style leader menu, optional)
#   VSpaceCode.whichkey                (which-key hints, optional)
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
