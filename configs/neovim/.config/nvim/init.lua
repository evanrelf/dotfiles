vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
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

vim.o.shiftwidth = 2
vim.bo.expandtab = true
vim.o.mouse = "a"

vim.cmd([[
  augroup autocmds
    autocmd FileType gitcommit setlocal colorcolumn=51,73
  augroup end
]])
