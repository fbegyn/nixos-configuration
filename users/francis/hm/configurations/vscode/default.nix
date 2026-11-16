{ pkgs, config, ... }:
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
#
# settings.json is a tracked JSON file symlinked into place (editable from VSCode).
let
  repoDir = "${config.home.homeDirectory}/nixos-configuration";
  settingsPath =
    if pkgs.stdenv.isDarwin then
      "Library/Application Support/Code/User/settings.json"
    else
      ".config/Code/User/settings.json";
in
{
  programs.vscode = {
    enable = true;
    package = pkgs.unstable.vscode;
    mutableExtensionsDir = true;

    profiles.default = {
      enableExtensionUpdateCheck = true;
      keybindings = import ./keybindings.nix;
    };
  };

  # settings.json is a plain tracked file, symlinked out-of-store so edits made
  # from within VSCode land directly in this repo.
  home.file."${settingsPath}".source =
    config.lib.file.mkOutOfStoreSymlink "${repoDir}/users/francis/hm/configurations/vscode/settings.json";
}
