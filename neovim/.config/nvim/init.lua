-- require('evan.packer')
require('evan.paq')
require('evan.options')
require('evan.mappings')

vim.api.nvim_exec([[
  augroup autocmds
    autocmd!
    autocmd FileType rust setlocal colorcolumn=81,101
    autocmd FileType gitcommit setlocal spell colorcolumn=51,73
  augroup END
]], false)
