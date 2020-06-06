" PLUGINS {{{1
call plug#begin()

" Appearance
Plug 'jeffkreeftmeijer/vim-numbertoggle'

" Movement
Plug 'critiqjo/husk-x.vim'

" Editing
Plug 'machakann/vim-sandwich'
Plug 'tomtom/tcomment_vim'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'

" Completion
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
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
Plug 'airblade/vim-gitgutter'

" Miscellaneous
Plug 'moll/vim-bbye'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'Konfekt/FastFold'
Plug 'tpope/vim-repeat'

call plug#end()


" PLUGIN SETTINGS {{{1

let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1

let g:polyglot_disabled = ['purescript']
let g:vim_markdown_conceal = 0
let g:vim_markdown_new_list_item_indent = 0
let g:haskell_indent_if = 2
let g:haskell_indent_in = 0
let g:purescript_indent_if = 2
let g:purescript_indent_in = 0

let g:gitgutter_map_keys = 0


" SETTINGS {{{1
" Appearance {{{2
colorscheme default
set background=light
set termguicolors
set number
set relativenumber
set colorcolumn=81
set noshowmode
set title
set splitbelow
set splitright
set shortmess=filmxTWIcF
set signcolumn=yes

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
command! Cd setlocal autochdir! | setlocal autochdir!


" MAPPINGS {{{1
noremap Y y$
noremap j gj
noremap k gk
nnoremap J m`J``
nnoremap > >>
nnoremap < <<
xnoremap < <gv
xnoremap > >gv
noremap n nzz
noremap N Nzz
noremap <silent> <Backspace> :<C-u>nohlsearch<CR>
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)
imap <Space> <Plug>(PearTreeSpace)

noremap gh 0
noremap gi ^
noremap gj G
noremap ge G$
noremap gk gg
noremap gl g_
" noremap U <C-r>
" noremap ' `
" nnoremap ~ vU
" nnoremap ` vu
