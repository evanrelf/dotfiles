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

" Movement {{{2
Plug 'wellle/targets.vim'
Plug 'critiqjo/husk-x.vim'
Plug 'junegunn/vim-slash'

" Intelligence {{{2
Plug 'lifepillar/vim-mucomplete'
" Plug 'neomake/neomake'
Plug 'w0rp/ale'
Plug 'sbdchd/neoformat'
" Plug 'cohama/lexima.vim'
" Plug 'rstacruz/vim-closer'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'alvan/vim-closetag'
Plug 'sickill/vim-pasta'

" Syntax {{{2
Plug 'sheerun/vim-polyglot'
Plug 'lervag/vimtex'
Plug 'eraserhd/parinfer-rust', { 'do': 'cargo build --release' }
Plug 'tpope/vim-sleuth'
Plug 'ntpeters/vim-better-whitespace'

" Miscellaneous {{{2
Plug 'vimlab/split-term.vim'
Plug 'tpope/vim-eunuch'
Plug 'Carpetsmoker/undofile_warn.vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'amiorin/vim-eval'
Plug 'tpope/vim-repeat'
Plug 'Konfekt/FastFold'

" }}}2

call plug#end()

" Plugin settings {{{2
" PaperColor
let g:PaperColor_Theme_Options = { 'theme': { 'default': { 'allow_bold': 0 } } }

" polyglot
let g:vim_markdown_new_list_item_indent = 0

" gitgutter
let g:gitgutter_map_keys = 0

" MUcomplete
set completeopt+=menuone,noselect
let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#delayed_completion = 1

" ALE
let g:ale_lint_on_text_changed = 'never'
" let g:ale_linters = { 'haskell': [] }

" Neomake
" call neomake#configure#automake('rw', 1000)

" polyglot
let g:polyglot_disabled = ['latex']

" vimtex
let g:vimtex_view_method = 'skim'

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
        \  highlight CursorLineNr NONE
        \| highlight link CursorLineNr Normal
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
set statusline+=\ \|\ %{strlen(&encoding)?&encoding:'-'}
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
inoremap <C-Tab> <Esc>:<C-u>bnext<CR>
inoremap <C-S-Tab> <Esc>:<C-u>bprev<CR>
tnoremap <C-Tab> <C-\><C-n>:<C-u>bnext<CR>
tnoremap <C-S-Tab> <C-\><C-n>:<C-u>bprev<CR>

tnoremap <Esc> <C-\><C-n>
tnoremap <C-w> <C-\><C-n><C-w>

" EasyAlign
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" vim-operator-flashy
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$

" Leader {{{2
map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nnoremap <Leader>g :%g/
xnoremap <Leader>g :g/
nnoremap <Leader>G :%g!/
xnoremap <Leader>G :g!/
nnoremap <Leader>= :<C-u>Neoformat<CR>
xnoremap <Leader>= :Neoformat<CR>

" FZF
noremap <Leader>h :<C-u>Helptags<CR>
noremap <Leader>f :<C-u>Files<CR>
noremap <Leader>/ :<C-u>BLines<CR>

" Terminal
noremap <Leader>t :<C-u>Term<CR>
noremap <Leader>T :<C-u>VTerm<CR>

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
        \  noremap <buffer> q :q<CR>
        \| nnoremap <buffer> <Esc> :q<CR>
  " Terminal
  autocmd TermOpen *
        \  setlocal nonumber norelativenumber
        \| noremap <buffer> <C-c> i<C-c>
        \| noremap <buffer> <C-d> i<C-d>
  autocmd BufEnter,WinEnter term://* startinsert
  " dirvish
  autocmd FileType dirvish
        \  noremap <buffer> u u
        \| noremap <buffer> <C-r> <C-r>
  " Man pages
  autocmd FileType man
        \  setlocal laststatus=0
        \| noremap <buffer> h <Nop>
        \| noremap <buffer> j <C-e>L0
        \| noremap <buffer> k <C-y>H0
        \| noremap <buffer> l <Nop>
  " plug
  autocmd FileType vim-plug setlocal nonumber norelativenumber
  " FZF
  autocmd FileType fzf
        \  nnoremap <buffer> <Esc> :q<CR>
        \| nnoremap <buffer> q :q<CR>
  " ALE
  autocmd FileType ale-preview setlocal wrap nonumber norelativenumber
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

augroup ResizeSplits " {{{2
  autocmd!
  autocmd VimResized * wincmd =
augroup END

" }}}2


" }}}1
