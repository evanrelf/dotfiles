" PLUGINS {{{1
call plug#begin()

" Appearance
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
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
colorscheme challenger_deep
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
set virtualedit=block,onemore
set lazyredraw
set updatetime=250
set inccommand=nosplit
set keywordprg=:help
scriptencoding 'utf-8'


" COMMANDS {{{1
command! V edit ~/.config/nvim/init.vim


" " MAPPINGS {{{1
" " noremap Y y$
" " noremap j gj
"
" nnoremap > >>
" nnoremap < <<
" xnoremap < <gv
" xnoremap > >gv
"
" nnoremap gh 0
" nnoremap gi ^
" nnoremap gj G
" nnoremap ge G$
" nnoremap gk gg
" nnoremap gl g_
" vnoremap gh <esc>0v
" vnoremap gi <esc>^v
" vnoremap gj <esc>Gv
" vnoremap ge <esc>G$v
" vnoremap gk <esc>ggv
" vnoremap gl <esc>g_v
" nnoremap Gh 0
" nnoremap Gi ^
" nnoremap Gj G
" nnoremap Ge G$
" nnoremap Gk gg
" nnoremap Gl g_
" vnoremap Gh 0
" vnoremap Gi ^
" vnoremap Gj G
" vnoremap Ge G$
" vnoremap Gk gg
" vnoremap Gl g_
"
" nmap # gcc
" xmap # gcgv
"
" " Normal mode
"
" augroup always_visual
"   autocmd!
"   autocmd VimEnter * normal! v
"   autocmd InsertLeave * normal! v
" augroup END
"
" nnoremap <esc> v
" xnoremap <esc> <nop>
"
" " Insert mode
"
" " Visual mode
"
" xnoremap ` ugv
" xnoremap ~ Ugv
" xnoremap q <esc>vb
" xnoremap Q b
" xnoremap w <esc>vw
" xnoremap W w
" xnoremap e <esc>ve
" xnoremap E e
" xnoremap r rgv
" xnoremap t <esc>vt
" xnoremap T t
" xnoremap y ygv
" xnoremap Y <nop>
" xnoremap u u
" xnoremap U <C-r>
" xnoremap i <esc>i
" xnoremap I <esc>I
" xnoremap o o
" xnoremap O O
" xnoremap h <esc>ghv
" xnoremap H gh
" xnoremap j <esc>gjv
" xnoremap J gj
" xnoremap k <esc>gkv
" xnoremap K gk
" xnoremap l <esc>glv
" xnoremap L gl
" xnoremap : :<C-u>
" xnoremap <A-;> o
" xnoremap x j$
" xnoremap X k$
