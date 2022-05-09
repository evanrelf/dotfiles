vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
  use({
    "ishan9299/modus-theme-vim",
    config = function()
      vim.opt.termguicolors = true
      vim.opt.background = "light"
      vim.cmd("colorscheme modus-operandi")
    end,
  })

  use({
    "numToStr/Comment.nvim",
    config = function()
      require("comment").setup()
      -- Use `gco` and `gcO` to explicitly continue comment on new lines
      vim.opt.formatoptions = vim.opt.formatoptions - {"o"}
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

vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.number = true
vim.opt.colorcolumn = "81"
vim.opt.mouse = "a"
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.api.nvim_create_augroup("filetype", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = "filetype",
  pattern = "fish",
  callback = function()
    vim.opt_local.shiftwidth = 4
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  group = "filetype",
  pattern = "rust",
  callback = function()
    vim.opt_local.colorcolumn = "81,101"
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  group = "filetype",
  pattern = "gitcommit",
  callback = function()
    vim.opt_local.colorcolumn = "51,73"
  end,
})
