vim.o.shiftwidth = 2
vim.bo.expandtab = true

vim.cmd([[
  augroup autocmds
    autocmd FileType gitcommit setlocal colorcolumn=51,73
  augroup end
]])
