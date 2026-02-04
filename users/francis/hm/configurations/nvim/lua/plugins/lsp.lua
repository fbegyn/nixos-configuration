-- LSP configuration (Neovim 0.11+ native API)
local capabilities = require("cmp_nvim_lsp").default_capabilities()

-- Fidget: LSP progress indicator
require("fidget").setup({})

-- LSP keymaps (attached per-buffer)
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("LspKeymaps", { clear = true }),
  callback = function(ev)
    local opts = function(desc)
      return { buffer = ev.buf, desc = desc }
    end
    local map = vim.keymap.set

    map("n", "gd", vim.lsp.buf.definition, opts("Go to definition"))
    map("n", "gD", vim.lsp.buf.declaration, opts("Go to declaration"))
    map("n", "gr", vim.lsp.buf.references, opts("References"))
    map("n", "gi", vim.lsp.buf.implementation, opts("Implementation"))
    map("n", "K", vim.lsp.buf.hover, opts("Hover"))
    map("n", "<C-k>", vim.lsp.buf.signature_help, opts("Signature help"))
    map("n", "<leader>ca", vim.lsp.buf.code_action, opts("Code action"))
    map("n", "<leader>cr", vim.lsp.buf.rename, opts("Rename"))
    map("n", "<leader>cf", function()
      vim.lsp.buf.format({ async = true })
    end, opts("Format"))
  end,
})

-- Go: format + organize imports on save
vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("GoFormatOnSave", { clear = true }),
  pattern = "*.go",
  callback = function()
    local params = vim.lsp.util.make_range_params()
    params.context = { only = { "source.organizeImports" } }
    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 3000)
    for _, res in pairs(result or {}) do
      for _, r in pairs(res.result or {}) do
        if r.edit then
          vim.lsp.util.apply_workspace_edit(r.edit, "utf-8")
        else
          vim.lsp.buf.execute_command(r.command)
        end
      end
    end
    vim.lsp.buf.format({ async = false })
  end,
})

-- Rust: format on save
vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("RustFormatOnSave", { clear = true }),
  pattern = "*.rs",
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})

-- Elixir: format on save
vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("ElixirFormatOnSave", { clear = true }),
  pattern = { "*.ex", "*.exs" },
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})

-- Python: format on save (via ruff)
vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("PythonFormatOnSave", { clear = true }),
  pattern = "*.py",
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})

-- Apply capabilities to all servers
vim.lsp.config("*", {
  capabilities = capabilities,
})

-- Server configurations
vim.lsp.config("gopls", {
  settings = {
    gopls = {
      staticcheck = true,
      analyses = {
        fillstruct = true,
      },
    },
  },
})

vim.lsp.config("rust_analyzer", {})

vim.lsp.config("bashls", {})

-- vim.lsp.config("ansiblels", {})

vim.lsp.config("ruff", {})

vim.lsp.config("nil_ls", {})

vim.lsp.config("elixirls", {
  cmd = { "elixir-ls" },
})

vim.lsp.config("pyright", {})

vim.lsp.config("denols", {})

vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      runtime = { version = "LuaJIT" },
      diagnostics = { globals = { "vim" } },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      telemetry = { enable = false },
    },
  },
})

-- Enable all configured servers
vim.lsp.enable({
  "gopls",
  "rust_analyzer",
  "nil_ls",
  "elixirls",
  "pyright",
  "denols",
  "lua_ls",
  "bashls",
  -- "ansiblels",
  "ruff",
})
