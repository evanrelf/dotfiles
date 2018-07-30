" PLUGINS {{{1
call plug#begin()

" Appearance {{{2
Plug 'NLKNguyen/papercolor-theme'
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'junegunn/goyo.vim'
Plug 'haya14busa/vim-operator-flashy' | Plug 'kana/vim-operator-user'

" Editing {{{2
Plug 'terryma/vim-multiple-cursors'
Plug 'machakann/vim-sandwich'
Plug 'tomtom/tcomment_vim'
Plug 'junegunn/vim-easy-align'

" Navigation {{{2
Plug 'justinmk/vim-dirvish'
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }

" Movement {{{2
Plug 'wellle/targets.vim'
Plug 'critiqjo/husk-x.vim'

" Intelligence {{{2
Plug 'lifepillar/vim-mucomplete'
Plug 'neomake/neomake'
Plug 'sbdchd/neoformat'
Plug 'cohama/lexima.vim'
Plug 'sickill/vim-pasta'

" Syntax {{{2
Plug 'sheerun/vim-polyglot'
Plug 'eraserhd/parinfer-rust', { 'do': 'cargo build --release' }

" Miscellaneous {{{2
Plug 'vimlab/split-term.vim'
Plug 'junegunn/vim-slash'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-sleuth'
Plug 'ntpeters/vim-better-whitespace'
Plug 'Carpetsmoker/undofile_warn.vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'tpope/vim-repeat'
Plug 'Konfekt/FastFold'

" }}}2

call plug#end()

" Plugin settings {{{2
" lightline
let g:lightline = { 'colorscheme': 'PaperColor' }

" gitgutter
let g:gitgutter_map_keys = 0

" MUcomplete
set completeopt+=menuone,noselect
let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#delayed_completion = 1

" Neomake
call neomake#configure#automake('rw', 1000)

" intero-neovim
let g:intero_type_on_hover = 1

" split-term
let g:disable_key_mappings = 1

" undofile_warn
let g:undofile_warn_mode = 2

" }}}2


" SETTINGS {{{1
" Apperance {{{2
set termguicolors
set background=dark
colorscheme PaperColor
set noshowmode
set title
set shortmess=filmxTWIc
set splitbelow
set splitright
set list
set listchars=tab:▸\ ,nbsp:␣
set number
set numberwidth=2
set colorcolumn=81

" Indentation {{{2
set expandtab
set tabstop=8
set softtabstop=2
set shiftwidth=2
set shiftround

" Formatting {{{2
set nowrap
set textwidth=79
set linebreak
set breakindent
set formatoptions=crqnj
set nojoinspaces

" Extra files {{{2
set undofile
set noswapfile
set nobackup
set nowritebackup

" Searching {{{2
set nohlsearch
set ignorecase
set smartcase
set gdefault
set report=0

" Wild mode {{{2
set wildmode=longest:full,full
set wildignore+=*/.git/*,*/tmp/*,*.swp,.DS_Store
set wildignorecase

" Miscellaneous {{{2
set hidden
set mouse=a
set virtualedit=block
set keywordprg=:help
set lazyredraw
set updatetime=100
set inccommand=nosplit

" }}}2


" COMMANDS {{{1
command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'
command! Bg let &background=(&background == "dark" ? "light" : "dark")


" MAPPINGS {{{1
" General {{{2
noremap ; :
noremap : ;
noremap Y y$
noremap j gj
noremap k gk
noremap gj j
noremap gk k
xnoremap > >gv
xnoremap < <gv
nnoremap J m`J``
xnoremap p "_dP
xnoremap P p

noremap Q @@
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]

nnoremap <Tab> zo
nnoremap <S-Tab> zc

tnoremap <Esc> <C-\><C-n>

" Leader {{{2
map <Space> <Leader>
map <Leader>y "+y
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nnoremap <Leader>g :%g/
xnoremap <Leader>g :g/
nnoremap <Leader>G :%g!/
xnoremap <Leader>G :g!/
noremap <Leader>bb :<C-u>Buffers<CR>
noremap <Leader><Tab> :<C-u>b#<CR>
noremap <Leader>bn :<C-u>bnext<CR>
noremap <Leader>bp :<C-u>bprev<CR>
noremap <Leader>bd :<C-u>bdelete<CR>
noremap <Leader>fs :<C-u>update<CR>
noremap <Leader>qq :<C-u>qa<CR>
noremap <Leader>h :<C-u>Helptags<CR>
noremap <Leader>f :<C-u>Files<CR>
noremap <Leader>/ :<C-u>BLines<CR>
noremap <Leader>t :<C-u>Term<CR>
noremap <Leader>T :<C-u>VTerm<CR>

" Plugins {{{2
" EasyAlign
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" vim-operator-flashy
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$

" Disabled {{{2
nnoremap U <Nop>
noremap S <Nop>
noremap X <Nop>
inoremap <C-c> <Nop>

" }}}2


" AUTOCOMMANDS {{{1
augroup FileTypeSettings " {{{2
  autocmd!
  " Haskell
  autocmd FileType haskell setlocal keywordprg=hoogle\ --info
  " Markdown
  autocmd FileType markdown setlocal wrap
  " Git
  autocmd FileType gitcommit setlocal textwidth=72 colorcolumn=73
  " Vim
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help
        \   noremap <buffer> q :q<CR>
        \ | nnoremap <buffer> <Esc> :q<CR>
  autocmd TermOpen *
        \   setlocal nonumber
        \ | setlocal norelativenumber
        \ | startinsert
  " Man pager
  autocmd FileType man
        \   setlocal laststatus=0
        \ | noremap <buffer> h <Nop>
        \ | noremap <buffer> j <C-e>L0
        \ | noremap <buffer> k <C-y>H0
        \ | noremap <buffer> l <Nop>
  " Plugins
  autocmd FileType vim-plug setlocal nonumber norelativenumber
  autocmd FileType fzf
        \   nnoremap <buffer> <Esc> :q<CR>
        \ | nnoremap <buffer> q :q<CR>
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

" }}}2


" vim: foldenable foldmethod=marker
" }}}1
