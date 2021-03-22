lua require('plugins')

set number
set relativenumber
set colorcolumn=81
set noshowmode
set nowrap
set splitbelow
set splitright
set wildmode=longest:full,list:full
set mouse=a
set expandtab
set shiftwidth=2
set shiftround
set nojoinspaces
set ignorecase
set smartcase
set wildignore+=*/.git/*,*/dist/*,*/dist-newstyle/*
set wildignorecase
set gdefault
set noswapfile
set nowritebackup
set hidden
set inccommand=nosplit
set virtualedit=block,onemore

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
