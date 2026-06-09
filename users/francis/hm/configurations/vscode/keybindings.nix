# Leader-Space chord map mirroring the emacs (general.el) and nvim (which-key)
# configs. Active only in vscode-neovim normal mode so insert-mode typing is
# untouched.
#
# If multi-step chords (e.g. "space f f") feel sluggish or unreliable, install
# VSpaceCode + WhichKey extensions — they render the menu and route the same
# chords with snappier feedback.
let
  normal = "editorTextFocus && neovim.mode == 'normal' && !inQuickOpen";
in
[
  # M-x / commands
  { key = "space space"; command = "workbench.action.showCommands"; when = normal; }

  # Files
  { key = "space f f"; command = "workbench.action.quickOpen"; when = normal; }
  { key = "space p f"; command = "workbench.action.quickOpen"; when = normal; }
  { key = "space p p"; command = "workbench.action.openRecent"; when = normal; }
  { key = "space f r"; command = "workbench.action.openRecent"; when = normal; }
  { key = "space f s"; command = "workbench.action.files.save"; when = normal; }

  # Buffers / editors
  { key = "space b b"; command = "workbench.action.showAllEditors"; when = normal; }
  { key = "space b d"; command = "workbench.action.closeActiveEditor"; when = normal; }
  { key = "space b n"; command = "workbench.action.nextEditor"; when = normal; }
  { key = "space b p"; command = "workbench.action.previousEditor"; when = normal; }

  # Windows / editor groups
  { key = "space w v"; command = "workbench.action.splitEditor"; when = normal; }
  { key = "space w s"; command = "workbench.action.splitEditorDown"; when = normal; }
  { key = "space w h"; command = "workbench.action.focusLeftGroup"; when = normal; }
  { key = "space w j"; command = "workbench.action.focusBelowGroup"; when = normal; }
  { key = "space w k"; command = "workbench.action.focusAboveGroup"; when = normal; }
  { key = "space w l"; command = "workbench.action.focusRightGroup"; when = normal; }
  { key = "space w d"; command = "workbench.action.closeEditorsInGroup"; when = normal; }

  # Search
  { key = "space s r"; command = "workbench.action.findInFiles"; when = normal; }
  { key = "space p g"; command = "workbench.action.findInFiles"; when = normal; }
  { key = "space s s"; command = "actions.find"; when = normal; }
  { key = "space s h"; command = "workbench.action.openGlobalKeybindings"; when = normal; }
  { key = "space s d"; command = "workbench.actions.view.problems"; when = normal; }
  { key = "space s k"; command = "workbench.action.openGlobalKeybindings"; when = normal; }

  # Code / LSP actions
  { key = "space c a"; command = "editor.action.quickFix"; when = normal; }
  { key = "space c r"; command = "editor.action.rename"; when = normal; }
  { key = "space c f"; command = "editor.action.formatDocument"; when = normal; }
  { key = "space c d"; command = "editor.action.showHover"; when = normal; }
  { key = "space c q"; command = "workbench.actions.view.problems"; when = normal; }

  # Git
  { key = "space g g"; command = "workbench.view.scm"; when = normal; }
  { key = "space g l"; command = "git-graph.view"; when = normal; }
  { key = "space g b"; command = "gitlens.toggleFileBlame"; when = normal; }
  { key = "space g p"; command = "editor.action.dirtydiff.next"; when = normal; }
  { key = "space g s"; command = "git.stageSelectedRanges"; when = normal; }
  { key = "space g u"; command = "git.unstageSelectedRanges"; when = normal; }
  { key = "space g R"; command = "git.revertSelectedRanges"; when = normal; }
  { key = "space g r"; command = "workbench.action.files.revert"; when = normal; }

  # Toggles
  { key = "space t t"; command = "workbench.action.toggleSidebarVisibility"; when = normal; }
  { key = "space t f"; command = "revealInExplorer"; when = normal; }

  # Terminal (matches SPC ' in emacs / nvim toggleterm)
  { key = "space '"; command = "workbench.action.terminal.toggleTerminal"; when = normal; }

  # Quit
  { key = "space q q"; command = "workbench.action.closeWindow"; when = normal; }
  { key = "space q w"; command = "workbench.action.files.saveAll"; when = normal; }

  # Diagnostics / hunks — [d ]d [h ]h are intentionally not bound here so
  # vscode-neovim forwards them to the real neovim mappings.
]
