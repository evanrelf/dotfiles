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
Plug 'NLKNguyen/papercolor-theme'
Plug 'cocopon/iceberg.vim'
Plug 'itchyny/lightline.vim'
Plug 'haya14busa/vim-operator-flashy' | Plug 'kana/vim-operator-user'

" Editing {{{2
Plug 'machakann/vim-sandwich'
Plug 'tomtom/tcomment_vim'
Plug 'junegunn/vim-easy-align'

" Movement {{{2
Plug 'junegunn/vim-slash'
Plug 'wellle/targets.vim'
Plug 'vim-utils/vim-husk'

" Navigation {{{2
Plug 'junegunn/fzf', { 'do': './install --bin' } | Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'
Plug 'scrooloose/nerdtree', { 'on': ['NERDTree', 'NERDTreeToggle'] }
Plug 'justinmk/vim-gtfo'
Plug 'airblade/vim-rooter'

" Intelligence {{{2
Plug 'w0rp/ale' | Plug 'maximbaz/lightline-ale'
Plug 'cohama/lexima.vim'

" Syntax {{{2
Plug 'sheerun/vim-polyglot'
Plug 'othree/yajs.vim'
Plug 'kid-icarus/vim-blockify'
Plug 'ap/vim-css-color'

" Miscellaneous {{{2
Plug 'tpope/vim-eunuch'
Plug 'pbrisbin/vim-mkdir'
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
let g:lightline = { 'colorscheme': 'PaperColor' }
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
" runtime macros/sandwich/keymap/surround.vim
let g:polyglot_disabled = ['javascript', 'jsx']
let g:undofile_warn_mode = 2
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
set background=light
colorscheme PaperColor
set noshowmode
set title
set shortmess=filmxTWIc
set splitbelow
set splitright
set list
set listchars=tab:▸\ ,nbsp:␣
set number
set colorcolumn=81

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
command! Bg let &background=(&background == "dark" ? "light" : "dark")


" MAPPINGS {{{1
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
noremap gb :let &background=(&background == "dark" ? "light" : "dark")<CR>

noremap <Tab> zo
noremap <S-Tab> zc

tnoremap <Esc> <C-\><C-n>

map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/

" EasyAlign
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" FZF
noremap <Leader><Space> :<C-u>Files<CR>
noremap <Leader>/ :<C-u>BLines<CR>
noremap <Leader>h :<C-u>Helptags<CR>
noremap <Leader>b :<C-u>Buffers<CR>

" ALE
map <Leader>en <Plug>(ale_next_wrap)
map <Leader>ep <Plug>(ale_previous_wrap)
map <Leader>ei <Plug>(ale_detail)

" vim-operator-flashy
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$

" Disabled
nnoremap U <Nop>
noremap M <Nop>
noremap S <Nop>
noremap X <Nop>
inoremap <C-c> <Nop>


" AUTOCOMMANDS {{{1
augroup FileTypeSettings " {{{2
  autocmd!
  " Vim
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help
        \   noremap <buffer> q :q<CR>
        \ | nnoremap <buffer> <Esc> :q<CR>
  autocmd FileType ale-preview setlocal wrap
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
  autocmd FileType haskell
        \   setlocal omnifunc=necoghc#omnifunc
        " \ | ALEDisable
        " \ | Ghcid
  " LÖVE
  autocmd FileType lua,moon nnoremap <buffer> K :silent !open . -a love.app<CR>
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

augroup NERDTree " {{{2
  autocmd!
  autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
augroup END

" }}}2


" }}}1
