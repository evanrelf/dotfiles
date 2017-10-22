" META {{{1
" vim: set foldenable foldmethod=marker

"  ███████ ██    ██  █████  ███    ██     ██████  ███████ ██      ███████
"  ██      ██    ██ ██   ██ ████   ██     ██   ██ ██      ██      ██
"  █████   ██    ██ ███████ ██ ██  ██     ██████  █████   ██      █████
"  ██       ██  ██  ██   ██ ██  ██ ██     ██   ██ ██      ██      ██
"  ███████   ████   ██   ██ ██   ████     ██   ██ ███████ ███████ ██


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

" Plugins {{{2
call plug#begin()

" Color schemes {{{3
Plug 'evanrelf/vim-deep-space'
" Plug 'junegunn/seoul256.vim'
" Plug 'tyrannicaltoucan/vim-quantum'
" Plug 'rakr/vim-two-firewatch'
" Plug 'atelierbram/Base2Tone-vim'
" Plug 'andreasvc/vim-256noir'
" Plug 'evanrelf/FlatColor' " Plug 'MaxSt/FlatColor'
" Plug 'KeitaNakamura/neodark.vim'
" Plug 'w0ng/vim-hybrid' | Plug 'cocopon/lightline-hybrid.vim'
" Plug 'dracula/vim'
" Plug 'morhetz/gruvbox'
" Plug 'ajh17/Spacegray.vim'
" Plug 'nanotech/jellybeans.vim'
" Plug 'zacanger/angr.vim'

" Appearance {{{3
Plug 'itchyny/lightline.vim'
Plug 'mgee/lightline-bufferline'
Plug 'myusuf3/numbers.vim'
" Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
Plug 'evanrelf/goyo.vim', { 'on': 'Goyo' }
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
Plug 'haya14busa/vim-operator-flashy' | Plug 'kana/vim-operator-user'

" Editing {{{3
Plug 'terryma/vim-multiple-cursors'
Plug 'terryma/vim-expand-region'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
Plug 'wellle/visual-split.vim'

" Auto-complete {{{3
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
        \ | Plug 'Shougo/neco-syntax'
        \ | Plug 'zchee/deoplete-clang'
        \ | Plug 'eagletmt/neco-ghc'
        \ | Plug 'pbogut/deoplete-elm'
        \ | Plug 'sebastianmarkow/deoplete-rust'
else
  Plug 'ajh17/VimCompletesMe'
endif
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'

" Analysis {{{3
Plug 'w0rp/ale'
Plug 'normenmueller/vim-hindent', { 'for': 'haskell' }
" Plug 'alx741/vim-hindent', { 'for': 'haskell' }
Plug 'rhysd/vim-clang-format', { 'for': ['c', 'cpp'] }
Plug 'mhinz/vim-signify'
Plug 'sickill/vim-pasta'
Plug 'metakirby5/codi.vim'

" Files {{{3
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'

" Syntax {{{3
Plug 'sheerun/vim-polyglot'
Plug 'elmcast/elm-vim'
Plug 'othree/yajs.vim'
Plug 'ap/vim-css-color'
Plug 'leafo/moonscript-vim'
Plug 'kid-icarus/vim-blockify'

" Utilities {{{3
Plug 'simnalamburt/vim-mundo', { 'on': ['MundoShow', 'MundoToggle'] }
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-unimpaired'
Plug 'ntpeters/vim-better-whitespace'
Plug 'Twinside/vim-hoogle', { 'on': 'Hoogle', 'for': 'haskell' }
Plug 'vim-utils/vim-husk'
Plug 'justinmk/vim-gtfo'
Plug 'junegunn/vim-slash'
Plug 'Carpetsmoker/undofile_warn.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'tpope/vim-repeat'
Plug 'Konfekt/FastFold'

" }}}

call plug#end()

" Plugin settings {{{2
" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 100
let g:deoplete#max_list = 10
let g:deoplete#sources#clang#clang_header = '/usr/local/Cellar/llvm/5.0.0/lib/clang'
let g:deoplete#sources#clang#libclang_path = '/usr/local/Cellar/llvm/5.0.0/lib/libclang.dylib'
let g:haskellmode_completion_ghc = 0
let g:necoghc_use_stack = 1
let g:necoghc_enable_detailed_browse = 1
let g:deoplete#sources#rust#racer_binary = $HOME . '/.cargo/bin/racer'
let g:deoplete#sources#rust#rust_source_path = $HOME . '/.rustup/rust-src/rust/src'
" lightline
let g:lightline = {}
let g:lightline = { 'colorscheme': 'deepspace' }
let g:lightline.tabline = { 'left': [['buffers']], 'right': [] }
let g:lightline.component_expand = { 'buffers': 'lightline#bufferline#buffers' }
let g:lightline.component_type = { 'buffers': 'tabsel' }
let g:lightline#bufferline#unnamed = '[No Name]'
" ale
let g:ale_lint_on_enter = 0
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
      \ 'haskell': ['stack-ghc', 'stack-build', 'stack-ghc-mod', 'hlint'],
      \ }
" delimitMate
let g:delimitMate_expand_cr = 1
let g:delimitMate_expand_space = 1
let g:delimitMate_balance_matchpairs = 1
let g:delimitMate_excluded_regions = 'Comment,String'
" auto-pairs
let g:AutoPairsCenterLine = 0
let g:AutoPairsMapCh = 0
" let g:AutoPairsMultilineClose = 0
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutFastWrap = ''
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutBackInsert = ''
" color schemes
let g:deepspace_italics = 1
let g:seoul256_background = 235
" other
let g:NERDTreeMinimalUI = 1
let g:mundo_playback_delay = 120
let g:dirvish_mode = ':sort ,^.*[\/], | silent keeppatterns g/.DS_Store/d'
let g:rustfmt_autosave = 1
let g:multi_cursor_exit_from_insert_mode = 0
let g:multi_cursor_exit_from_visual_mode = 0
let g:clang_format#auto_format = 1
let g:gtfo#terminals = { 'mac': 'iterm' }
let g:polyglot_disabled = ['javascript', 'jsx', 'graphql', 'elm']
let g:undofile_warn_mode = 2
let g:signify_vcs_list = ['git']

" }}}


" SETTINGS {{{1
" Neovim defaults {{{2
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

" Neovim-exclusive {{{2
if has('nvim')
  set inccommand=nosplit
  tnoremap <Esc> <C-\><C-n>
endif

" Appearance {{{2
set background=dark
colorscheme deep-space
set termguicolors
set showtabline=2
set number
set relativenumber
set numberwidth=5
set colorcolumn=80
set list
set listchars=tab:▸\ ,nbsp:␣
set fillchars+=vert:│
set splitright
set splitbelow
set noequalalways
set showcmd
set cmdheight=2
set noshowmode
set shortmess=filmxTWc
set wildmode=longest:full,full
set wildignorecase
set title

" Indentation {{{2
set smartindent
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

" Wrapping {{{2
set nowrap
set textwidth=79
set linebreak
set breakindent
set showbreak=↪\ 
set nojoinspaces

" Search and replace {{{2
set nohlsearch
set ignorecase
set smartcase
set gdefault

" Folds {{{2
set foldenable
set foldmethod=marker

" Buffers {{{2
set hidden
set switchbuf=useopen,usetab

" Backup/undo/swap files {{{2
set undofile
set noswapfile
set nobackup
set nowritebackup

" Performance {{{2
set synmaxcol=200
set lazyredraw
set ttimeout
set ttimeoutlen=10
set wildignore+=*/.git/*,*/tmp/*,*.swp,.DS_Store


" GUI {{{2
if has('gui_running')
  set guifont=Iosevka:h15
  set guioptions=g
  set guicursor=n-v-ve:block-Cursor-blinkon0
  set linespace=1

  if has('gui_macvim')
    set macligatures
    set macmeta
    " set macthinstrokes
  endif
endif

" Other {{{2
set mouse=a
scriptencoding utf-8
set isfname-==
set virtualedit=block

" }}}


" COMMANDS & FUNCTIONS {{{1
" :V - Open $MYVIMRC
command! V edit $MYVIMRC
" :Cd - Change current working directory to where the file is located
command! Cd setlocal autochdir | setlocal noautochdir
" :Marked - Open buffer in 'Marked 2.app'
command! Marked silent !open % -a 'Marked 2.app'
" :PlugOpen - Open plugin's GitHub page in web browser
command! PlugOpen silent normal! ^"zyi':!open https://github.com/z<CR>

" :AutoSave[!] - Enable and disable auto saving of buffer changes {{{2
" From junegunn: https://git.io/v5FPw
function! s:autosave(enable)
  augroup autosave
    autocmd!
    if a:enable
      autocmd TextChanged,InsertLeave <buffer>
            \  if empty(&buftype) && !empty(bufname(''))
            \|   silent! update
            \| endif
      echom 'Auto save enabled'
    else
      echom 'Auto save disabled'
    endif
  augroup END
endfunction

command! -bang AutoSave call s:autosave(<bang>1)

" :A - Switch between source and header files {{{2
" From junegunn: https://git.io/v5FX3
function! A(cmd)
  let l:name = expand('%:r')
  let l:ext = tolower(expand('%:e'))
  let l:sources = ['c', 'cc', 'cpp', 'cxx']
  let l:headers = ['h', 'hh', 'hpp', 'hxx']
  for l:pair in [[l:sources, l:headers], [l:headers, l:sources]]
    let [l:set1, l:set2] = l:pair
    if index(l:set1, l:ext) >= 0
      for l:h in l:set2
        let l:aname = l:name . '.' . l:h
        for l:a in [l:aname, toupper(l:aname)]
          if filereadable(l:a)
            execute a:cmd l:a
            return
          end
        endfor
      endfor
    endif
  endfor
endfunction

command! A call A('e')


" gx - Open web browser to plugin page on GitHub
function! Plug_gx()
  let l:line = getline('.')
  let l:sha = matchstr(l:line, '^  \X*\zs\x\{7,9}\ze ')
  let l:name = empty(l:sha) ? matchstr(l:line, '^[-x+] \zs[^:]\+\ze:')
                      \ : getline(search('^- .*:$', 'bn'))[2:-2]
  let l:uri = get(get(g:plugs, l:name, {}), 'uri', '')
  if l:uri !~# 'github.com'
    return
  endif
  let l:repo = matchstr(l:uri, '[^:/]*/' . l:name)
  let l:url = empty(l:sha) ? 'https://github.com/'.l:repo
                         \ : printf('https://github.com/%s/commit/%s', l:repo, l:sha)
  call netrw#BrowseX(l:url, 0)
endfunction

augroup PlugGx
  autocmd!
  autocmd FileType vim-plug nnoremap <buffer> <silent> gx :call Plug_gx()<CR>
augroup END

" Custom fold text {{{2
function! CustomFoldText()
  let l:line = getline(v:foldstart)
  let l:windowWidth = winwidth(0) - (&foldcolumn + (&number * &numberwidth))
  let l:foldedLines = (v:foldend - v:foldstart)

  " EDIT THIS
  let l:filler = '-'

  if &filetype ==# 'vim'
    if l:line =~# '^\s*"'
      " Remove comment character
      let l:line = substitute(l:line, '^\s*"\s*', '', '')
      " Remove fold marker
      let l:line = substitute(l:line, '{\{3\}\d\s*$', '', '')
    else
      " Remove comment character and fold marker
      let l:line = substitute(l:line, '\s*"\s*{\{3\}\d\?', '', '')
    endif
  endif

  if &filetype ==# 'fish'
    if l:line =~# '^\s*#'
      " Remove comment character
      let l:line = substitute(l:line, '^\s*#\s*', '', '')
      " Remove fold marker
      let l:line = substitute(l:line, '{\{3\}\d\s*$', '', '')
    else
      " Remove comment character and fold marker
      let l:line = substitute(l:line, '\s*#\s*{\{3\}\d\?', '', '')
    endif
  endif

  " Remove trailing whitespace
  let l:line = substitute(l:line, '\s*$', '', '')

  " EDIT THIS
  let l:lineText = repeat(l:filler, 2) . ' ' . l:line . ' '

  " EDIT THIS
  if l:foldedLines == 1
    let l:x = 'line'
  else
    let l:x = 'lines'
  endif
  let l:foldedLinesText = ' ' . l:foldedLines . ' ' . l:x . ' ' . repeat(l:filler, 2)

  let l:fillerWidth = 2 + (l:windowWidth - strdisplaywidth(l:lineText) - (strdisplaywidth(l:foldedLinesText) + 3)) / strdisplaywidth(l:filler)
  return l:lineText . repeat(l:filler, l:fillerWidth + 1) . l:foldedLinesText
endfunction
set foldtext=CustomFoldText()

" Multiple cursor stuff {{{2
function! g:Multiple_cursors_before()
  let g:deoplete#disable_auto_complete=1
endfunction

function! g:Multiple_cursors_after()
  let g:deoplete#disable_auto_complete=0
endfunction

" }}}

" MAPPINGS {{{1
" Disabled/available {{{2
" Enters :ex mode from normal mode (never desired!)
noremap Q <Nop>
" Reverts change on last edited line, but doesn't add to the undo tree...?
nnoremap U <Nop>
" Positions cursor at the middle of the screen (goes with H and L)
noremap M <Nop>
" cc
nnoremap S <Nop>
" hx
noremap X <Nop>
" :x
noremap ZZ <Nop>
" l
noremap <Space> <Nop>


" Better defaults {{{2
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

" Uncategorized {{{2
noremap H ^
noremap L g_
noremap gH H
noremap gL L
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]
noremap <C-l> :redraw!<CR>

" Moving lines {{{2
nnoremap <silent> <A-j> :setlocal foldmethod=manual<CR>:m .+1<CR>==:setlocal foldmethod=marker<CR>
nnoremap <silent> <A-k> :setlocal foldmethod=manual<CR>:m .-2<CR>==:setlocal foldmethod=marker<CR>
xnoremap <silent> <A-j> <Esc>:setlocal foldmethod=manual<CR>'<V'>:m '>+1<CR>gv=:setlocal foldmethod=marker<CR>gv
xnoremap <silent> <A-k> <Esc>:setlocal foldmethod=manual<CR>'<V'>:m '<-2<CR>gv=:setlocal foldmethod=marker<CR>gv
xnoremap <silent> J <Esc>:setlocal foldmethod=manual<CR>'<V'>:m '>+1<CR>gv=:setlocal foldmethod=marker<CR>gv
xnoremap <silent> K <Esc>:setlocal foldmethod=manual<CR>'<V'>:m '<-2<CR>gv=:setlocal foldmethod=marker<CR>gv

" Folding {{{2
noremap <Tab> zo
noremap <S-Tab> zc

" Switching tabs/buffers {{{2
noremap <silent> <C-Tab> :bnext<CR>
noremap <silent> <C-S-Tab> :bprev<CR>

" Leader {{{2
map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
noremap <Leader>ff :Files<CR>


" Plugins {{{2
" mundo
noremap <Leader>u :MundoToggle<CR>
" NERDTree
noremap <Leader>t :NERDTreeToggle<CR>
noremap <Leader>\ :NERDTreeToggle<CR>
" visual-split
xmap V <Plug>(Visual-Split-VSSplitAbove)
xmap R <Plug>(Visual-Split-VSResize)
" easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
" operator-flashy
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$
if has('nvim')
  " deoplete
  inoremap <silent> <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
  inoremap <silent> <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
else
  " VimCompletesMe
  inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" }}}


" AUTOCOMMANDS {{{1
augroup FiletypeSettings " {{{2
  autocmd!
  " Vim
  autocmd BufWritePost $MYVIMRC nested silent source $MYVIMRC
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType vim-plug setlocal nonumber
  " Git
  autocmd FileType gitcommit
        \   setlocal spell
        \ | setlocal textwidth=72
  " Markdown
  autocmd FileType markdown
        \   setlocal wrap
        \ | nnoremap <buffer> <Leader>1 m`"zyy"zpVr=``
        \ | nnoremap <buffer> <Leader>2 m`"zyy"zpVr-``
        \ | nnoremap <buffer> <Leader>3 I### <Esc>l
        \ | nnoremap <buffer> <Leader>4 I#### <Esc>l
        \ | nnoremap <buffer> <Leader>5 I##### <Esc>l
  " C++
  " autocmd FileType cpp
  "       \  echom 'Your config here'
  " Haskell
  autocmd FileType haskell
        \   setlocal omnifunc=necoghc#omnifunc
        \ | setlocal keywordprg=:Hoogle
        \ | nnoremap <buffer> <Leader>h :Hoogle<Space>
        \ | nnoremap <buffer> <Leader>r :IronRepl<CR>
  " Elm
  autocmd FileType elm
        \  setlocal tabstop=4 softtabstop=4 shiftwidth=4
  " " Rust
  " autocmd FileType rust
  "       \  setlocal tabstop=4 softtabstop=4 shiftwidth=4
augroup END

augroup FormatOptions " {{{2
  " https://stackoverflow.com/a/23326474/1664444
  autocmd!
  autocmd BufEnter * setlocal formatoptions=cqnj
augroup END

augroup Plugins " {{{2
  autocmd!
  " deoplete
  autocmd CompleteDone * silent! pclose!
  " NERDTree
  autocmd BufEnter * if winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree() | q | endif
augroup END

" }}}


