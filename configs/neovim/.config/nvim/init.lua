local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      local configs = require("nvim-treesitter.configs")
      configs.setup({
        ensure_installed = {
          "bash", "comment", "css", "diff", "dockerfile", "fish", "haskell",
          "haskell_persistent", "html", "javascript", "json", "lua", "make",
          "markdown", "markdown_inline", "nix", "prql", "purescript", "python",
          "rust", "sql", "toml", "tsx", "typescript", "vim", "xml", "yaml",
        },
        highlight = { enable = true },
        indent = { enable = true },
      })
    end,
  },
  { "michaeljsmith/vim-indent-object" },
  {
    "projekt0n/github-nvim-theme",
    priority = 1000,
    config = function()
      vim.cmd("colorscheme github_light")
      vim.cmd("highlight! link StatusLine CursorLineNr")
    end,
  },
  { "wsdjeg/vim-fetch" },
}, {
  install = {
    colorscheme = { "github_light" },
  },
})

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.colorcolumn = "81"
vim.opt.showmode = false
vim.opt.laststatus = 3
vim.opt.cmdheight = 0
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.wrap = false
vim.opt.mousescroll = "ver:3,hor:0"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.gdefault = true
vim.opt.wildmode = "longest:full,full"
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.swapfile = false

vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("x", "<", "<gv")
vim.keymap.set("x", ">", ">gv")
