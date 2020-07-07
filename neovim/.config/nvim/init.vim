call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'sheerun/vim-polyglot'
Plug 'sonph/onehalf', {'rtp': 'vim/'}
call plug#end()

set mouse=a
set termguicolors
set background=light
colorscheme onehalflight
set number
set relativenumber
set colorcolumn=81
set expandtab
set shiftwidth=2

nnoremap Y y$

augroup filetype
  autocmd!
  autocmd FileType gitcommit setlocal colorcolumn=73
augroup END
