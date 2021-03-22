lua require('plugins')
lua require('options')
lua require('mappings')

augroup autocmds
  autocmd!
  autocmd FileType rust setlocal colorcolumn=81,101
  autocmd FileType gitcommit setlocal spell colorcolumn=51,73
augroup END
