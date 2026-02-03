-- Orgmode
require("orgmode").setup({
  org_agenda_files = { "~/org/**/*" },
  org_default_notes_file = "~/org/inbox.org",
  org_capture_templates = {
    t = {
      description = "Todo",
      template = "* TODO %?\n  %i\n  %a",
      target = "~/org/inbox.org",
      headline = "Tasks",
    },
    s = {
      description = "Standup",
      template = "* %?\nEntered on %U\n  %i\n  %a",
      target = "~/org/standup.org",
      datetree = true,
    },
  },
})

vim.keymap.set("n", "<leader>cc", function()
  require("orgmode").action("capture.prompt")
end, { desc = "Org capture" })
