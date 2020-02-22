" PLUGINS {{{1
call plug#begin()

" Appearance
Plug 'chriskempson/base16-vim'
Plug 'jeffkreeftmeijer/vim-numbertoggle'

" Movement
Plug 'critiqjo/husk-x.vim'

" Editing
Plug 'machakann/vim-sandwich'
Plug 'tomtom/tcomment_vim'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'

" Completion
Plug 'tmsvg/pear-tree'

" Formatting
Plug 'sbdchd/neoformat', { 'on': ['Neoformat'] }
Plug 'junegunn/vim-easy-align'
Plug 'sickill/vim-pasta'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-sleuth'

" Syntax
Plug 'sheerun/vim-polyglot'
Plug 'evanrelf/purescript-vim'
Plug 'vmchale/dhall-vim'

" Files
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'tpope/vim-eunuch'

" Miscellaneous
Plug 'moll/vim-bbye'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'Konfekt/FastFold'
Plug 'tpope/vim-repeat'

call plug#end()


" SETTINGS {{{1
" Appearance {{{2
set background=dark
colorscheme base16-gruvbox-dark-hard
set termguicolors
set number
set relativenumber
set colorcolumn=81
set noshowmode
set title
set splitbelow
set splitright

" Indentation {{{2
set expandtab
set softtabstop=2
set shiftwidth=2
set shiftround

" Formatting {{{2
set nowrap
set linebreak
set breakindent
set nojoinspaces

" Extra files {{{2
set noswapfile
set nobackup
set nowritebackup

" Searching {{{2
set hlsearch
set ignorecase
set smartcase
set gdefault
set report=0

" Wild mode {{{2
set wildmode=longest:full,full
set wildignore+=*/.git/*,.DS_Store
set wildignorecase

" Miscellaneous {{{2
set mouse=a
set hidden
set virtualedit=block
set lazyredraw
set updatetime=250
set inccommand=nosplit
set keywordprg=:help
scriptencoding 'utf-8'


" MAPPINGS {{{1
noremap Y y$
noremap j gj

" nnoremap > >>
" nnoremap < <<
xnoremap < <gv
xnoremap > >gv

noremap gh 0
noremap gi ^
noremap gj G
noremap ge G$
noremap gk gg
noremap gl g_

nmap # gcc
xmap # gcgv
