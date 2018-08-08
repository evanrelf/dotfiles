" META {{{1
" vim: foldenable foldmethod=marker

" Evan Relf's Neovim config
" https://github.com/evanrelf/dotfiles/


" PLUGINS {{{1
call plug#begin()

" Appearance {{{2
Plug 'NLKNguyen/papercolor-theme'
Plug 'airblade/vim-gitgutter'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'junegunn/goyo.vim'
Plug 'haya14busa/vim-operator-flashy' | Plug 'kana/vim-operator-user'

" Editing {{{2
Plug 'machakann/vim-sandwich'
Plug 'tomtom/tcomment_vim'
Plug 'junegunn/vim-easy-align'
Plug 'michaeljsmith/vim-indent-object'

" Navigation {{{2
Plug 'justinmk/vim-dirvish'
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'qpkorr/vim-bufkill'

" Movement {{{2
Plug 'wellle/targets.vim'
Plug 'critiqjo/husk-x.vim'
Plug 'junegunn/vim-slash'

" Intelligence {{{2
Plug 'lifepillar/vim-mucomplete'
Plug 'neomake/neomake'
Plug 'sbdchd/neoformat'
Plug 'cohama/lexima.vim'
Plug 'sickill/vim-pasta'

" Syntax {{{2
Plug 'sheerun/vim-polyglot'
Plug 'eraserhd/parinfer-rust', { 'do': 'cargo build --release' }
Plug 'tpope/vim-sleuth'
Plug 'ntpeters/vim-better-whitespace'

" Miscellaneous {{{2
Plug 'vimlab/split-term.vim'
Plug 'tpope/vim-eunuch'
Plug 'Carpetsmoker/undofile_warn.vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'tpope/vim-repeat'
Plug 'Konfekt/FastFold'

" }}}2

call plug#end()

" Plugin settings {{{2
" PaperColor
let g:PaperColor_Theme_Options = { 'theme': { 'default': { 'allow_bold': 0 } } }

" lightline
let g:lightline = { 'colorscheme': 'PaperColor' }

" gitgutter
let g:gitgutter_map_keys = 0

" MUcomplete
set completeopt+=menuone,noselect
let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#delayed_completion = 1

" Neomake
" call neomake#configure#automake('rw', 1000)

" split-term
let g:disable_key_mappings = 1

" undofile_warn
let g:undofile_warn_mode = 2

" }}}2


" SETTINGS {{{1
" Apperance {{{2
augroup ColorSchemes " {{{2
  autocmd!
  autocmd ColorScheme *
        \   highlight CursorLineNr NONE
        \ | highlight link CursorLineNr Normal
augroup END " }}}2
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

" Status line {{{2
set statusline=
set statusline+=%#Cursor#
" Mode
set statusline+=\ %{SL_Mode()}
set statusline+=\ %#CursorLine#
" File path
set statusline+=\ %f\ %m\%r
" Separator
set statusline+=%=
" File format
set statusline+=\ %{&fileformat}
" File encoding
set statusline+=\ \|\ %{strlen(&fileencoding)?&fileencoding:'-'}
" Filetype
set statusline+=\ \|\ %{strlen(&filetype)?&filetype:'-'}
set statusline+=\ %#Cursor#
" Cursor position
set statusline+=\ %3l:%-3v

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


" FUNCTIONS {{{1
function! SL_Mode() abort
  let l:mode = mode()
  if l:mode ==# 'n'
    return 'NORMAL'
  elseif l:mode ==# 'i'
    return 'INSERT'
  elseif l:mode ==# 'v'
    return 'VISUAL'
  elseif l:mode ==# 'V'
    return "V-LINE"
  elseif l:mode ==# ''
    return "V-BLOCK"
  elseif l:mode ==# 'R'
    return 'REPLACE'
  elseif l:mode ==# 'c'
    return 'COMMAND'
  elseif l:mode ==# 't'
    return 'TERMINAL'
  else
    return l:mode
  endif
endfunction

function! CommandCabbr(abbreviation, expansion) abort
  silent execute 'cabbrev ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ Command call CommandCabbr(<f-args>)


" COMMANDS {{{1
command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'
command! Bg let &background=(&background == "dark" ? "light" : "dark")
command! Ghcid 60VTerm ghcid

Command w update


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

noremap <C-Tab> :<C-u>bnext<CR>
noremap <C-S-Tab> :<C-u>bprev<CR>
inoremap <C-Tab> :<C-u>bnext<CR>
inoremap <C-S-Tab> :<C-u>bprev<CR>

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

" FZF
noremap <Leader>h :<C-u>Helptags<CR>
noremap <Leader>f :<C-u>Files<CR>
noremap <Leader>/ :<C-u>BLines<CR>

" Terminal
noremap <Leader>t :<C-u>Term<CR>
noremap <Leader>T :<C-u>VTerm<CR>

" Spacemacs
noremap <Leader>bb :<C-u>Buffers<CR>
noremap <Leader><Tab> :<C-u>b#<CR>
noremap <Leader>bn :<C-u>BF<CR>
noremap <Leader>bp :<C-u>BB<CR>
noremap <Leader>bd :<C-u>BUN<CR>
noremap <Leader>qq :<C-u>qa<CR>

" Plugins {{{2
" EasyAlign
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" vim-operator-flashy
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$

" Neoformat
xnoremap gQ :Neoformat<CR>

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
        \ | noremap <buffer> <C-c> I<C-c>
  " dirvish
  autocmd FileType dirvish
        \   noremap <buffer> u u
        \ | noremap <buffer> <C-r> <C-r>
  " Man pages
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


" }}}1
