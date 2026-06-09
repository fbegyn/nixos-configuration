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
  "editor.acceptSuggestionOnEnter" = "off";

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
  };
  "rust-analyzer.check.command" = "clippy";
  "nix.enableLanguageServer" = true;
  "nix.serverPath" = "nil";
  "python.languageServer" = "Pylance";
  "ruff.organizeImports" = true;
  "deno.enable" = false;
  "deno.lint" = true;

  # vscode-neovim — extension auto-detects nvim on PATH; uncomment if detection fails.
  # "vscode-neovim.neovimExecutablePaths.darwin" = "/etc/profiles/per-user/francis/bin/nvim";
  # "vscode-neovim.neovimExecutablePaths.linux" = "/etc/profiles/per-user/francis/bin/nvim";

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
}
