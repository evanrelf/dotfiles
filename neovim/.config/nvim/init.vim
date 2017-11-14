" vim: foldenable foldmethod=marker
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
" 2}}}

call plug#begin()

" Appearance
Plug 'cocopon/iceberg.vim'
Plug 'evanrelf/vim-deep-space'
Plug 'itchyny/lightline.vim'
Plug 'evanrelf/goyo.vim'
Plug 'haya14busa/vim-operator-flashy' | Plug 'kana/vim-operator-user'

" Editing
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'terryma/vim-expand-region'
Plug 'junegunn/vim-easy-align'
Plug 'dhruvasagar/vim-table-mode'

" Movement
Plug 'junegunn/vim-slash'
Plug 'wellle/targets.vim'
Plug 'vim-utils/vim-husk'

" Navigation
Plug 'junegunn/fzf', { 'do': './install --bin' } | Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-gtfo'

" Auto-complete
Plug 'cohama/lexima.vim'
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim' | Plug 'roxma/nvim-yarp' | Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'eagletmt/neco-ghc'
Plug 'zchee/deoplete-clang'

" Languages
Plug 'w0rp/ale' | Plug 'maximbaz/lightline-ale'
Plug 'sheerun/vim-polyglot'
Plug 'othree/yajs.vim'
Plug 'elmcast/elm-vim'
Plug 'kid-icarus/vim-blockify'

" Miscellaneous
Plug 'tpope/vim-eunuch'
Plug 'ntpeters/vim-better-whitespace'
Plug 'sickill/vim-pasta'
Plug 'Carpetsmoker/undofile_warn.vim'
Plug 'tpope/vim-repeat'
Plug 'Konfekt/FastFold'

call plug#end()


" PLUGIN SETTINGS {{{2
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
let g:ale_fix_on_save = 1
let g:ale_linters = {
\   'haskell': ['stack_build', 'stack_ghc', 'stack-ghc-mod', 'ghc-mod', 'hlint', 'hfmt']
\ }
let g:ale_fixers = {
\   'cpp': ['clang-format']
\ }

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
" 2}}}


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
" 2}}}

" Apperance
set termguicolors
set background=dark
colorscheme iceberg
set noshowmode
set title
set shortmess=filmxTWIc
set splitbelow
set splitright

" Indentation
set smartindent
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set shiftround

" Formatting
set nowrap
set textwidth=79
set linebreak
set breakindent
set formatoptions=crqnj
set nojoinspaces

" Extra files
set undofile
set noswapfile
set nobackup
set nowritebackup

" Searching
set nohlsearch
set ignorecase
set smartcase
set gdefault
set report=0

" Wild mode
set wildmode=longest:full,full
set wildignore+=*/.git/*,*/tmp/*,*.swp,.DS_Store
set wildignorecase

" Miscellaneous
set lazyredraw
set hidden
set mouse=a
set virtualedit=block
set inccommand=nosplit
set keywordprg=:help


" COMMANDS {{{1
command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Reload source $MYVIMRC
command! -range PlugOpen silent normal! ^"zyi':!open https://github.com/z<CR>
command! Marked silent !open % -a 'Marked 2.app'


" MAPPINGS {{{1
" Disabled
noremap U <Nop>
noremap M <Nop>
noremap S <Nop>
noremap X <Nop>
inoremap <C-c> <Nop>

" Improved defaults
noremap ; :
noremap : ;
noremap Y y$
noremap j gj
noremap k gk
noremap gj j
noremap gk k
noremap ' `
nnoremap J m`J``
tnoremap <Esc> <C-\><C-n>

" Personal preference
noremap Q @@
xnoremap > >gv
xnoremap < <gv
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]
xnoremap p "_dP
xnoremap P p

" Tab
noremap <Tab> zo
noremap <S-Tab> zc
noremap <silent> <C-Tab> :bnext<CR>
noremap <silent> <C-S-Tab> :bprev<CR>

" Leader
map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nmap <Leader>; m`gcc``
xmap <Leader>; gcgv
noremap <Leader>b :ls<CR>:b<Space>

" Plugin
noremap <Leader><Space> :<C-u>Files<CR>
noremap <Leader>/ :<C-u>Lines<CR>
noremap <Leader>h :<C-u>Helptags<CR>
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$
map <Leader>en <Plug>(ale_next_wrap)
map <Leader>ep <Plug>(ale_previous_wrap)
map <Leader>ei <Plug>(ale_detail)
inoremap <silent> <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <silent> <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"



" AUTOCOMMANDS {{{1
augroup WebDev
  autocmd BufLeave *.css,*.scss normal! mC
  autocmd BufLeave *.html normal! mH
  autocmd BufLeave *.js normal! mJ
augroup END

augroup FileTypeSettings
  autocmd!
  " Vim
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help
        \   noremap <buffer> q :q<CR>
        \ | nnoremap <Esc> :q<CR>
  " Plugins
  autocmd FileType vim-plug setlocal nonumber norelativenumber
  autocmd FileType fzf
        \   nnoremap <buffer> <Esc> :q<CR>
        \ | nnoremap <buffer> q :q<CR>
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
augroup END

augroup FormatOptions
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END
