{
  # Theme & UI
  "workbench.colorTheme" = "Gruvbox Dark Hard";
  "workbench.iconTheme" = "vscode-icons";
  "workbench.editor.enablePreview" = false;
  "workbench.startupEditor" = "none";
  "window.titleBarStyle" = "custom";

  # Font (matches Menlo on macOS / DejaVu Sans Mono on Linux per base-init.el)
  "editor.fontFamily" = "Menlo, 'DejaVu Sans Mono', 'Terminess Nerd Font Mono', monospace";
  "editor.fontSize" = 14;
  "editor.fontLigatures" = true;
  "editor.lineHeight" = 1.4;

  # Editor surface (mirrors nvim init.lua + emacs base-init.el)
  "editor.lineNumbers" = "relative";
  "editor.rulers" = [ 101 ];
  "editor.cursorSurroundingLines" = 8;
  "editor.cursorBlinking" = "solid";
  "editor.renderWhitespace" = "trailing";
  "editor.bracketPairColorization.enabled" = true;
  "editor.guides.bracketPairs" = "active";
  "editor.guides.indentation" = true;
  "editor.minimap.enabled" = false;
  "editor.scrollBeyondLastLine" = false;
  "editor.smoothScrolling" = true;
  "editor.tabSize" = 2;
  "editor.insertSpaces" = true;
  "editor.detectIndentation" = true;
  "editor.wordWrap" = "off";
  "editor.suggestSelection" = "first";
  # corfu parity: RET inserts the selected candidate; Tab also accepts.
  "editor.acceptSuggestionOnEnter" = "smart";
  # corfu auto-completion feel: auto-delay 0.1s.
  "editor.quickSuggestionsDelay" = 100;
  # emacs disables the LSP documentHighlightProvider — no symbol-under-cursor glow.
  "editor.occurrencesHighlight" = "off";

  # Files
  "files.trimTrailingWhitespace" = true;
  "files.insertFinalNewline" = true;
  "files.trimFinalNewlines" = true;
  "files.autoSave" = "off";

  # Format on save
  "editor.formatOnSave" = true;
  "editor.formatOnPaste" = false;

  # Per-language overrides (mirror format-on-save behavior from emacs eglot + nvim LspAttach)
  "[go]" = {
    "editor.formatOnSave" = true;
    "editor.codeActionsOnSave" = {
      "source.organizeImports" = "explicit";
    };
  };
  "[rust]" = {
    "editor.formatOnSave" = true;
    "editor.defaultFormatter" = "rust-lang.rust-analyzer";
  };
  "[elixir]" = {
    "editor.formatOnSave" = true;
  };
  "[python]" = {
    "editor.formatOnSave" = true;
    "editor.defaultFormatter" = "charliermarsh.ruff";
    "editor.codeActionsOnSave" = {
      "source.organizeImports.ruff" = "explicit";
      "source.fixAll.ruff" = "explicit";
    };
  };
  "[nix]" = {
    "editor.formatOnSave" = false;
    "editor.tabSize" = 2;
  };
  "[lua]" = {
    "editor.tabSize" = 2;
  };
  "[yaml]" = {
    "editor.tabSize" = 2;
    "editor.insertSpaces" = true;
  };
  "[markdown]" = {
    "editor.wordWrap" = "on";
    "editor.quickSuggestions" = {
      "comments" = "off";
      "strings" = "off";
      "other" = "off";
    };
  };

  # Language servers
  "go.useLanguageServer" = true;
  "go.toolsManagement.autoUpdate" = false;
  "gopls" = {
    "ui.semanticTokens" = true;
    "ui.completion.usePlaceholders" = true;
    "ui.diagnostic.staticcheck" = true;      # eglot :staticcheck t
    "analyses" = { "fillstruct" = true; };   # eglot :analyses (:fillstruct t)
  };
  "rust-analyzer.check.command" = "clippy";
  "nix.enableLanguageServer" = true;
  "nix.serverPath" = "nil";
  # nil flake settings mirror fb/eglot-workspace-config: keep flake inputs from
  # being auto-evaluated/archived (fb/nil-auto-eval defaults to nil).
  "nix.serverSettings" = {
    "nil" = {
      "nix" = {
        "flake" = {
          "autoEvalInputs" = false;
          "autoArchive" = false;
        };
      };
    };
  };
  "python.languageServer" = "Pylance";
  "ruff.organizeImports" = true;
  "deno.enable" = false;
  "deno.lint" = true;

  # vscode-neovim — extension auto-detects nvim on PATH; uncomment if detection fails.
  "vscode-neovim.neovimExecutablePaths.darwin" = "/etc/profiles/per-user/francis/bin/nvim";
  "vscode-neovim.neovimExecutablePaths.linux" = "/etc/profiles/per-user/francis/bin/nvim";

  # Terminal
  "terminal.integrated.fontFamily" = "Menlo, 'DejaVu Sans Mono', 'Terminess Nerd Font Mono', monospace";
  "terminal.integrated.fontSize" = 14;
  "terminal.integrated.defaultProfile.osx" = "zsh";
  "terminal.integrated.cursorBlinking" = false;
  "terminal.integrated.smoothScrolling" = true;

  # Search
  "search.useIgnoreFiles" = true;
  "search.smartCase" = true;
  "search.useGlobalIgnoreFiles" = true;

  # Git
  "git.autofetch" = true;
  "git.confirmSync" = false;
  "git.openRepositoryInParentFolders" = "always";
  "diffEditor.ignoreTrimWhitespace" = false;

  # Telemetry off
  "telemetry.telemetryLevel" = "off";
  "redhat.telemetry.enabled" = false;

  # Misc
  "explorer.confirmDelete" = false;
  "explorer.confirmDragAndDrop" = false;
  "explorer.compactFolders" = false;
  "breadcrumbs.enabled" = true;
  "zenMode.hideLineNumbers" = false;
  "update.mode" = "none";

  # ── VSpaceCode / WhichKey (spacemacs-style leader menu) ──
  # The `space` key triggers `whichkey.show` (see keybindings.nix). VSpaceCode's
  # default menu already covers most f/b/w/g chords; the overrides below align
  # it with the emacs general.el leader map (SPC …). NOTE: emacs SPC j
  # (jujutsu / magit) has no first-class VSCode equivalent — use the SCM view or
  # terminal instead.
  "whichkey.delay" = 250;              # emacs which-key-idle-delay 0.25
  "whichkey.sortOrder" = "alphabetically";
  "vspacecode.bindingOverrides" = [
    # single leaves
    { keys = "'";   name = "Terminal";       type = "command"; command = "workbench.action.terminal.toggleTerminal"; }
    { keys = "t f"; name = "Fullscreen";      type = "command"; command = "workbench.action.toggleFullScreen"; }
    { keys = "t t"; name = "File tree";       type = "command"; command = "workbench.action.toggleSidebarVisibility"; }
    { keys = "s i"; name = "Symbols (imenu)"; type = "command"; command = "workbench.action.gotoSymbol"; }
    { keys = "s s"; name = "Search line";     type = "command"; command = "actions.find"; }
    { keys = "s r"; name = "Ripgrep";         type = "command"; command = "workbench.action.findInFiles"; }
    { keys = "f d"; name = "Reveal in tree";  type = "command"; command = "revealInExplorer"; }
    # +code submenu (emacs SPC c …)
    { keys = "c"; name = "+code"; type = "bindings"; bindings = [
        { key = "a"; name = "Code actions"; type = "command"; command = "editor.action.quickFix"; }
        { key = "d"; name = "Diagnostics";  type = "command"; command = "workbench.actions.view.problems"; }
        { key = "n"; name = "Next error";   type = "command"; command = "editor.action.marker.next"; }
        { key = "p"; name = "Prev error";   type = "command"; command = "editor.action.marker.prev"; }
        { key = "r"; name = "Rename";       type = "command"; command = "editor.action.rename"; }
        { key = "f"; name = "Format";       type = "command"; command = "editor.action.formatDocument"; }
      ]; }
    # +project extras (emacs SPC p k/b/t)
    { keys = "p k"; name = "Kill buffers";  type = "command"; command = "workbench.action.closeAllEditors"; }
    { keys = "p b"; name = "Switch buffer"; type = "command"; command = "workbench.action.showAllEditors"; }
    { keys = "p t"; name = "Terminal";      type = "command"; command = "workbench.action.terminal.toggleTerminal"; }
    # +git (emacs SPC g g/l/b)
    { keys = "g"; name = "+git"; type = "bindings"; bindings = [
        { key = "g"; name = "Status"; type = "command"; command = "workbench.view.scm"; }
        { key = "l"; name = "Log";    type = "command"; command = "git-graph.view"; }
        { key = "b"; name = "Blame";  type = "command"; command = "gitlens.toggleFileBlame"; }
      ]; }
    # +remote (emacs SPC r … / TRAMP → Remote-SSH)
    { keys = "r"; name = "+remote"; type = "bindings"; bindings = [
        { key = "f"; name = "Connect host"; type = "command"; command = "opensshremotes.openEmptyWindow"; }
      ]; }
  ];
}
