vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
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
        highlight = {
          enable = true,
          disable = {"haskell", "markdown"},
        },
        indent = { enable = true },
      })
    end,
  })

  use({
    "rktjmp/lush.nvim",
    config = function()
      vim.o.termguicolors = true
      vim.o.background = "light"
      package.loaded["primer"] = nil
      require("lush")(require("primer"))
    end,
  })

  use({
    "wbthomason/packer.nvim",
    config = function()
      vim.api.nvim_exec([[
        augroup packer
          autocmd!
          autocmd BufWritePost init.lua source <afile> | PackerCompile
        augroup end
      ]], false)
    end,
  })
end)

vim.o.expandtab = true
vim.o.shiftwidth = 2
vim.o.number = true
vim.o.mouse = "a"

vim.cmd([[
  augroup autocmds
    autocmd FileType gitcommit setlocal colorcolumn=51,73
  augroup end
]])
