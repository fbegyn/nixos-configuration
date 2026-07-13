# Leader = Space. Under VSpaceCode + WhichKey the leader is a single trigger that
# opens the spacemacs-style popup; the actual SPC chord map lives in settings.nix
# (`vspacecode.bindingOverrides`) so it mirrors the emacs general.el leader map.
#
# The trigger is scoped to vscode-neovim normal/visual modes so insert-mode typing
# is untouched. Diagnostic/hunk motions ([d ]d [h ]h) are intentionally left to
# neovim's own mappings.
let
  normal = "editorTextFocus && neovim.mode == 'normal' && !inQuickOpen";
  visual = "editorTextFocus && neovim.mode == 'visual' && !inQuickOpen";
in
[
  { key = "space"; command = "whichkey.show"; when = normal; }
  { key = "space"; command = "whichkey.show"; when = visual; }
]
