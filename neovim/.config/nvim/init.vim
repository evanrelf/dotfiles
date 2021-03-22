lua require('plugins')
lua require('options')

nnoremap Y y$
nnoremap j gj
nnoremap k gk
xnoremap < <gv
xnoremap > >gv
noremap <silent> <Backspace> :<C-u>setlocal hlsearch!<CR>
nnoremap \| :%!
xnoremap \| :!
noremap U :<C-u>echoerr "Use \<C-r\>"<CR>

augroup autocmds
  autocmd!
  autocmd FileType rust setlocal colorcolumn=81,101
  autocmd FileType gitcommit setlocal spell colorcolumn=51,73
augroup END
