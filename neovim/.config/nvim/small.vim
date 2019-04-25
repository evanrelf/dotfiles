" PLUGINS {{{1
if exists('*plug#begin')
  call plug#begin()
  Plug 'bluz71/vim-moonfly-colors'
  Plug 'machakann/vim-sandwich'
  Plug 'tpope/vim-commentary'
  Plug 'welle/targets.vim'
  Plug 'sheerun/vim-polyglot'
  Plug 'tpope/vim-sleuth'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'tpope/vim-repeat'
  call plug#end()
endif

" SETTINGS {{{1
" Appearance
try
  colorscheme moonfly
catch
  colorscheme darkblue
endtry
set termguicolors
set number
" set relativenumber
" set cursorline
" set colorcolumn=81,101
set noshowmode
set rulerformat=%7(%3(%l%),%3(%c%V%)%)
set shortmess=filmxTWIcF
set title
set splitbelow
set splitright
set scrolloff=2

" Indentation
set expandtab
set softtabstop=2
set shiftwidth=2
set shiftround

" Formatting
set nowrap
set linebreak
set breakindent
set nojoinspaces
set formatoptions=cqnj

" Extra files
set noswapfile
set nowritebackup

" Searching
set ignorecase
set smartcase
set gdefault
set report=0

" Wild mode
set wildmode=longest:full,full
set wildignore+=*/.git/*,*/.stack-work/*
set wildignorecase

" Miscellaneous
set mouse=a
set hidden
set virtualedit=block
set lazyredraw
set updatetime=100
set inccommand=nosplit


" COMMANDS {{{1
command! V edit $HOME/.config/v2/init.vim


" MAPPINGS {{{1
noremap Y y$
noremap j gj
noremap k gk
nnoremap J m`J``
nnoremap < <<
nnoremap > >>
xnoremap < <gv
xnoremap > >gv
nnoremap gp `[v`]
noremap H ^
noremap L g_
noremap <Backspace> :<C-u>noh<CR>

" Leader
map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
xnoremap <Leader>S :sort<CR>


" AUTO COMMANDS {{{1
augroup FormatOptions
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END


" vim: foldenable foldmethod=marker
