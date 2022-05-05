vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
  use({
    "ishan9299/modus-theme-vim",
    config = function()
      vim.o.termguicolors = true
      vim.o.background = "light"
      vim.cmd("colorscheme modus-operandi")
    end,
  })

  use({
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "bash", "c", "cpp", "css", "dockerfile", "dot", "elixir", "fish",
          "go", "haskell", "html", "javascript", "json", "lua", "make",
          "markdown", "nix", "perl", "python", "ruby", "rust", "toml",
          "typescript", "vim", "yaml", "zig",
        },
        highlight = { enable = true },
        indent = {
          enable = true,
          disable = {"haskell", "markdown"},
        },
      })
    end,
  })

  use({
    "wbthomason/packer.nvim",
    config = function()
      vim.api.nvim_create_augroup("packer", { clear = true })
      vim.api.nvim_create_autocmd("BufWritePost", {
        group = "packer",
        pattern = "init.lua",
        command = "source <afile> | PackerCompile",
      })
    end,
  })
end)

vim.o.expandtab = true
vim.o.shiftwidth = 2
vim.o.number = true
vim.o.colorcolumn = "81"
vim.o.mouse = "a"

vim.api.nvim_create_augroup("filetype", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = "filetype",
  pattern = "fish",
  command = "setlocal shiftwidth=4",
})
vim.api.nvim_create_autocmd("FileType", {
  group = "filetype",
  pattern = "rust",
  command = "setlocal colorcolumn=81,101",
})
vim.api.nvim_create_autocmd("FileType", {
  group = "filetype",
  pattern = "gitcommit",
  command = "setlocal colorcolumn=51,73",
})
