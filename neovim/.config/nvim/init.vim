" META {{{1
" vim: foldenable foldmethod=marker

" Evan Relf's Neovim config
" https://github.com/evanrelf/dotfiles/


" PLUGINS {{{1
" Auto-install vim-plug {{{2
if has('nvim')
  let g:plug_dir = $HOME . '/.config/nvim'
else
  let g:plug_dir = $HOME . '/.vim'
endif

if empty(glob(g:plug_dir . '/autoload/plug.vim'))
  let g:choice = confirm('Install vim-plug?', "&Yes\n&No")
  if g:choice == 1
    echomsg 'Installing vim-plug...'
    silent execute '!curl -fLo ' . g:plug_dir . '/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    augroup InstallVimPlug
      autocmd!
      autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    augroup END
  else
    echomsg 'Install skipped'
  endif
endif
" }}}2

call plug#begin()

" Appearance {{{2
Plug 'cocopon/iceberg.vim'
Plug 'itchyny/lightline.vim'
Plug 'evanrelf/goyo.vim'
Plug 'haya14busa/vim-operator-flashy' | Plug 'kana/vim-operator-user'

" Editing {{{2
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'junegunn/vim-easy-align'
Plug 'dhruvasagar/vim-table-mode'

" Movement {{{2
Plug 'junegunn/vim-slash'
Plug 'wellle/targets.vim'
Plug 'vim-utils/vim-husk'

" Navigation {{{2
Plug 'junegunn/fzf', { 'do': './install --bin' } | Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-gtfo'

" Auto-complete {{{2
Plug 'cohama/lexima.vim'
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim' | Plug 'roxma/nvim-yarp' | Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'eagletmt/neco-ghc'
Plug 'zchee/deoplete-clang'

" Syntax {{{2
Plug 'w0rp/ale' | Plug 'maximbaz/lightline-ale'
Plug 'sheerun/vim-polyglot'
Plug 'othree/yajs.vim'
Plug 'elmcast/elm-vim'
Plug 'leafo/moonscript-vim'
Plug 'ssteinbach/vim-pico8-syntax'
Plug 'kid-icarus/vim-blockify'
Plug 'ap/vim-css-color'

" Miscellaneous {{{2
Plug 'mhinz/vim-sayonara'
Plug 'tpope/vim-eunuch'
Plug 'pbrisbin/vim-mkdir'
Plug 'wincent/terminus'
Plug 'ntpeters/vim-better-whitespace'
Plug 'sickill/vim-pasta'
Plug 'Carpetsmoker/undofile_warn.vim'
Plug 'tpope/vim-repeat'
Plug 'Konfekt/FastFold'

" }}}2

call plug#end()

" Plugin settings {{{2
" deoplete
let g:deoplete#enable_at_startup = 1
let g:necoghc_use_stack = 1
let g:necoghc_enable_detailed_browse = 1
let g:deoplete#sources#clang#clang_header = '/usr/local/Cellar/llvm/5.0.0/lib/clang'
let g:deoplete#sources#clang#libclang_path = '/usr/local/Cellar/llvm/5.0.0/lib/libclang.dylib'

" lightline
let g:lightline = {}
let g:lightline = { 'colorscheme': 'iceberg' }
let g:lightline.component_expand = {
      \   'linter_warnings': 'lightline#ale#warnings',
      \   'linter_errors': 'lightline#ale#errors',
      \   'linter_ok': 'lightline#ale#ok',
      \ }
let g:lightline.component_type = {
      \   'linter_warnings': 'warning',
      \   'linter_errors': 'error',
      \ }
let g:lightline.active = { 'right': [['lineinfo'], ['percent'], ['linter_errors', 'linter_warnings'], ['fileformat', 'fileencoding', 'filetype']] }

" ale
let g:ale_sign_column_always = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
\   'haskell': ['stack-ghc-mod', 'hlint', 'stack-build', 'stack-ghc']
\ }
let g:ale_fixers = {
\   'cpp': ['clang-format']
\ }
let g:ale_lua_luacheck_options = '--std _G+love'

" fzf
let g:fzf_layout = { 'down': '~20%' }
let g:fzf_colors = {
  \   'fg':      ['fg', 'Normal'],
  \   'bg':      ['bg', 'Normal'],
  \   'hl':      ['fg', 'Comment'],
  \   'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \   'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \   'hl+':     ['fg', 'Statement'],
  \   'info':    ['fg', 'PreProc'],
  \   'border':  ['fg', 'Ignore'],
  \   'prompt':  ['fg', 'Conditional'],
  \   'pointer': ['fg', 'Exception'],
  \   'marker':  ['fg', 'Keyword'],
  \   'spinner': ['fg', 'Label'],
  \   'header':  ['fg', 'Comment']
  \ }

" Other
let g:polyglot_disabled = ['javascript', 'jsx', 'graphql', 'elm']
let g:undofile_warn_mode = 2
let g:table_mode_corner="|"
" }}}2


" SETTINGS {{{1
" Neovim defaults for Vim {{{2
if !has('nvim')
  if !isdirectory($HOME . '/.vim/backup')
    silent call mkdir($HOME . '/.vim/backup', 'p')
  endif
  if !isdirectory($HOME . '/.vim/undo')
    silent call mkdir($HOME . '/.vim/undo', 'p')
  endif
  if !isdirectory($HOME . '/.vim/swap')
    silent call mkdir($HOME . '/.vim/swap', 'p')
  endif
  set undodir=~/.vim/undo
  set backupdir=.,~/.vim/backup
  set directory=~/.vim/swap//
  set autoindent
  set autoread
  set backspace=indent,eol,start
  set belloff=all
  set complete-=i
  set display=lastline
  " set formatoptions=tcqj
  set history=10000
  set hlsearch
  set incsearch
  set langnoremap
  set nolangremap
  set laststatus=2
  " set listchars=tab:>\ ,trail:-,nbsp:+"
  " Enabled implicitly when a .vimrc file exists
  " set nocompatible
  set nrformats=bin,hex
  set ruler
  set sessionoptions-=options
  set showcmd
  set smarttab
  set tabpagemax=50
  set tags=./tags;,tags
  set ttyfast
  set viminfo+=!
  set wildmenu
endif

" Apperance {{{2
set termguicolors
set background=dark
colorscheme iceberg
set number
set noshowmode
set title
set shortmess=filmxTWIc
set splitbelow
set splitright
set list
set listchars=tab:▸\ ,nbsp:␣

" Indentation {{{2
set smartindent
set expandtab
set tabstop=2
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
set lazyredraw
set hidden
set mouse=a
set virtualedit=block
set keywordprg=:help

if has('nvim')
  set inccommand=nosplit
endif

" }}}2


" COMMANDS {{{1
command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Reload source $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'


" MAPPINGS {{{1
" Leader {{{2
map <Space> <Leader>
noremap <Leader>q :Sayonara<CR>
noremap <Leader>Q :Sayonara!<CR>
noremap <Leader>w :update<CR>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nmap <Leader>; m`gcc``
xmap <Leader>; gcgv
noremap <Leader>b :ls<CR>:b<Space>
noremap <Leader><Space> :<C-u>Files<CR>
noremap <Leader>/ :<C-u>Lines<CR>
noremap <Leader>h :<C-u>Helptags<CR>
map <Leader>en <Plug>(ale_next_wrap)
map <Leader>ep <Plug>(ale_previous_wrap)
map <Leader>ei <Plug>(ale_detail)

" Normal & Visual {{{2
noremap ; :
noremap : ;
noremap Y y$
noremap j gj
noremap k gk
noremap gj j
noremap gk k
noremap ' `
nnoremap gp `[v`]
noremap Q @@
noremap <Tab> zo
noremap <S-Tab> zc
noremap <silent> <C-Tab> :<C-u>bnext<CR>
noremap <silent> <C-S-Tab> :<C-u>bprev<CR>
noremap <C-l> :<C-u>redraw!<CR>
map y <Plug>(operator-flashy)

" Normal {{{2
nnoremap J m`J``
nmap ga <Plug>(EasyAlign)
nmap Y <Plug>(operator-flashy)$

" Visual {{{2
xnoremap > >gv
xnoremap < <gv
xnoremap gp <Esc>`[v`]
xnoremap p "_dP
xnoremap P p
xmap ga <Plug>(EasyAlign)

" Insert {{{2
inoremap <C-c> <Nop>
inoremap <C-l> <C-o>:redraw!<CR>
inoremap <silent> <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <silent> <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Command {{{2


" Terminal {{{2
tnoremap <Esc> <C-\><C-n>

" Disabled {{{2
nnoremap U <Nop>
noremap M <Nop>
noremap S <Nop>
noremap X <Nop>

" }}}2

" AUTOCOMMANDS {{{1
augroup WebDev " {{{2
  autocmd!
  autocmd BufLeave *.css,*.scss normal! mC
  autocmd BufLeave *.html normal! mH
  autocmd BufLeave *.js normal! mJ
augroup END

augroup FileTypeSettings " {{{2
  autocmd!
  " Vim
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help
        \   noremap <buffer> q :q<CR>
        \ | nnoremap <buffer> <Esc> :q<CR>
  " Plugins
  autocmd FileType vim-plug setlocal nonumber norelativenumber
  autocmd FileType fzf
        \   nnoremap <buffer> <Esc> :q<CR>
        \ | nnoremap <buffer> q :q<CR>
  " Man pages
  autocmd FileType man
        \   setlocal laststatus=0
        \ | map <buffer> J <C-e>
        \ | map <buffer> K <C-y>
  " Git
  autocmd FileType gitcommit setlocal textwidth=72 colorcolumn=73
  " Markdown
  autocmd FileType markdown
        \   setlocal wrap
        \ | nnoremap <silent> <buffer> <Leader>1 :s/\v^#+ //e<CR>I# <Esc>
        \ | nnoremap <silent> <buffer> <Leader>2 :s/\v^#+ //e<CR>I## <Esc>
        \ | nnoremap <silent> <buffer> <Leader>3 :s/\v^#+ //e<CR>I### <Esc>
        \ | nnoremap <silent> <buffer> <Leader>4 :s/\v^#+ //e<CR>I#### <Esc>
  " Haskell
  autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
  " LÖVE
  autocmd FileType lua,moon nnoremap <buffer> K :silent !open . -a love.app<CR>
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

" }}}2


" }}}1
