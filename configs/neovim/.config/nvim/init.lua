-- Color scheme
vim.cmd("colorscheme flexoki")

-- Indentation
vim.o.expandtab = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"fish", "python", "rust", "zig"},
  callback = function()
    vim.bo.shiftwidth = 4
    vim.bo.tabstop = 4
  end,
})

-- Line length
vim.o.wrap = false
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"go", "make"},
  callback = function()
    vim.bo.expandtab = false
    vim.bo.shiftwidth = 0
    vim.bo.tabstop = 4
  end,
})
vim.o.colorcolumn = "81"
vim.api.nvim_create_autocmd("FileType", {
  pattern = "rust",
  callback = function()
    vim.wo.colorcolumn = "101"
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"gitcommit", "jjdescription"},
  callback = function()
    vim.wo.colorcolumn = "51,73"
    vim.bo.textwidth = 72
  end,
})

-- Search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Tree Sitter
vim.pack.add({
  "https://github.com/nvim-treesitter/nvim-treesitter",
})
require("nvim-treesitter").install({
  "bash", "fish", "go", "haskell", "haskell_persistent", "json", "lua",
  "markdown", "nix", "rust", "sql", "zig",
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "sh", "fish", "go", "haskell", "json", "lua", "markdown", "nix", "rust",
    "sql", "zig",
  },
  callback = function()
    vim.treesitter.start()
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
