lua require('evan.plugins')
lua require('evan.options')
lua require('evan.mappings')

augroup autocmds
  autocmd!
  autocmd FileType rust setlocal colorcolumn=81,101
  autocmd FileType gitcommit setlocal spell colorcolumn=51,73
augroup END
