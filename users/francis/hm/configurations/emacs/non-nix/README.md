# Standalone Emacs config (no nix)

Reuses `../base-init.el` and `../early-init.el` with a separately installed
Emacs 30+ (e.g. `brew install emacs-plus` / `emacs-mac`).

## Running

From the repo root:

    users/francis/hm/configurations/emacs/non-nix/emacs-nn [-nw] [file...]

Optionally put it on your PATH:

    ln -s ~/nixos-configuration/users/francis/hm/configurations/emacs/non-nix/emacs-nn ~/.local/bin/emacs-nn

Without the script:

    emacs --init-directory=$HOME/nixos-configuration/users/francis/hm/configurations/emacs/non-nix

To pick a specific Emacs (e.g. macOS GUI build):

    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs ./emacs-nn

## Notes

- Packages are installed from GNU/NonGNU/MELPA on first start (slow once);
  majutsu via `package-vc`.
- Run `M-x fb/nn-install-grammars` once for tree-sitter (needs a C compiler).
- State lives in `~/.local/share/emacs-non-nix/`; `~/.emacs.d`,
  `~/.config/emacs` and the home-manager setup are not touched.
- LSP servers/tools (gopls, nil, ruff, ...) must be on PATH (e.g. via brew).
