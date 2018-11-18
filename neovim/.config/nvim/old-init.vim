" META {{{1
" vim: foldenable foldmethod=marker

" Evan Relf's Neovim config
" https://github.com/evanrelf/dotfiles/


" PLUGINS {{{1
call plug#begin()

" Appearance {{{2
Plug 'rakr/vim-one'
Plug 'lifepillar/vim-solarized8'
Plug 'evanrelf/papercolor-theme'
Plug 'cocopon/iceberg.vim'
Plug 'itchyny/lightline.vim'
Plug 'jeffkreeftmeijer/vim-numbertoggle'

" Editing {{{2
Plug 'terryma/vim-multiple-cursors'
Plug 'machakann/vim-sandwich'
Plug 'tpope/vim-commentary'
Plug 'matze/vim-move'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-abolish'

" Navigation {{{2
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }

" Movement {{{2
Plug 'wellle/targets.vim'
Plug 'critiqjo/husk-x.vim'
Plug 'junegunn/vim-slash'

" Intelligence {{{2
" Plug 'zxqfl/tabnine-vim'
Plug 'w0rp/ale'
Plug 'sbdchd/neoformat'
Plug 'airblade/vim-gitgutter'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'alvan/vim-closetag'
Plug 'sickill/vim-pasta'

" Syntax {{{2
Plug 'sheerun/vim-polyglot'
Plug 'ap/vim-css-color'
Plug 'lervag/vimtex'
Plug 'tpope/vim-sleuth', { 'on': [] } " Doesn't get along with vim-polyglot
Plug 'ntpeters/vim-better-whitespace'

" Miscellaneous {{{2
Plug 'vimlab/split-term.vim'
Plug 'moll/vim-bbye'
Plug 'tpope/vim-eunuch'
Plug 'pbrisbin/vim-mkdir'
Plug 'tpope/vim-repeat'
Plug 'vim-scripts/visualrepeat'
Plug 'Konfekt/FastFold'

" }}}2

call plug#end()

" Plugin settings {{{2
" PaperColor
let g:PaperColor_Theme_Options = { 'theme': { 'default': { 'allow_bold': 0 } } }

" lightline
let g:lightline = { 'colorscheme': 'one' }

" gitgutter
let g:gitgutter_map_keys = 0

" vim-better-whitespace
let g:better_whitespace_filetypes_blacklist = ['vim-plug', 'help']

" ALE
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = { 'haskell': ['stack_build', 'hlint'] }
let g:ale_fixers = { 'haskell': ['hlint'] }

" Neoformat
let g:neoformat_basic_format_trim = 1

" polyglot
let g:polyglot_disabled = ['latex']
let g:vim_markdown_new_list_item_indent = 0
let g:elm_setup_keybindings = 0
let g:elm_format_autosave = 0

" }}}2


" SETTINGS {{{1
" Apperance {{{2
" augroup ColorSchemes
"   autocmd!
"   autocmd ColorScheme *
"         \  highlight CursorLineNr NONE
"         \| highlight link CursorLineNr Normal
" augroup END
set termguicolors
set background=dark
colorscheme one
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
set noswapfile
set nobackup
set nowritebackup

" Searching {{{2
set nohlsearch
set ignorecase
set smartcase
set gdefault
set report=0
set grepprg=rg\ --vimgrep

" Wild mode {{{2
set wildmode=longest:full,full
set wildignore+=*/.git/*,*/tmp/*,*.swp,.DS_Store
set wildignorecase

" Miscellaneous {{{2
set hidden
set autoread
set mouse=nv
set virtualedit=block
set keywordprg=:help
set lazyredraw
set updatetime=100
set inccommand=nosplit

" }}}2


" FUNCTIONS {{{1
function! CommandCabbr(abbreviation, expansion) abort
  silent execute 'cabbrev ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ Command call CommandCabbr(<f-args>)


" COMMANDS {{{1
command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'
command! Bg let &background=(&background == 'dark' ? 'light' : 'dark')

" Command w update
Command bd Bd

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'options': '--delimiter : --nth 4..' },
      \   <bang>0)

command! -bang -nargs=* GRg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'dir': systemlist('git rev-parse --show-toplevel')[0],
      \     'options': '--delimiter : --nth 4..' },
      \   <bang>0)

command! -bang -nargs=? -complete=dir FilesP
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)


" MAPPINGS {{{1
" General {{{2
" noremap ; :
" noremap : ;
noremap Y y$
noremap j gj
noremap k gk
xnoremap > >gv
xnoremap < <gv
nnoremap J m`J``

noremap Q @@
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]

nnoremap <Tab> zo
nnoremap <S-Tab> zc

tnoremap <Esc> <C-\><C-n>

" EasyAlign
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" vim-slash
map <plug>(slash-after) zz

" Leader {{{2
map <Space> <Leader>
noremap <Leader>w :<C-u>write<CR>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nnoremap <Leader>g :%g/
xnoremap <Leader>g :g/
nnoremap <Leader>= :<C-u>Neoformat<CR>
xnoremap <Leader>= :Neoformat<CR>

" ALE
noremap <silent> <Leader>ap :<C-u>ALEPrevious<CR>
noremap <silent> <Leader>an :<C-u>ALENext<CR>
noremap <silent> <Leader>ad :<C-u>ALEDetail<CR>
noremap <silent> <Leader>af :<C-u>ALEFix<CR>

" FZF
noremap <silent> <Leader>f :<C-u>GFiles<CR>
noremap <silent> <Leader>F :<C-u>Files<CR>
noremap <silent> <Leader>r :<C-u>GRg<CR>
noremap <silent> <Leader>R :<C-u>Rg<CR>
noremap <silent> <Leader>h :<C-u>Helptags<CR>
noremap <silent> <Leader>b :<C-u>Buffers<CR>

" Terminal
noremap <silent> <Leader>t :<C-u>Term<CR>
noremap <silent> <Leader>T :<C-u>VTerm<CR>

" GUI {{{2
if has('gui_vimr')
  noremap <silent> <C-Tab> :<C-u>tabnext<CR>
  noremap <silent> <C-S-Tab> :<C-u>tabprev<CR>
endif

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
  " C++
  autocmd FileType cpp setlocal commentstring=//\ %s
  " Markdown
  autocmd FileType markdown setlocal wrap
  " Git
  autocmd FileType gitcommit setlocal textwidth=72 colorcolumn=73
  autocmd BufEnter .gitconfig*
        \  setlocal filetype=gitconfig
        \| setlocal noexpandtab shiftwidth=8
  " Docker
  autocmd BufEnter Dockerfile* setlocal ft=Dockerfile
  " Vim
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help
        \  noremap <buffer> q :q<CR>
        \| nnoremap <buffer> <Esc> :q<CR>
  " Terminal
  autocmd TermOpen * setlocal nonumber norelativenumber
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
  autocmd FileType ale-preview
        \  setlocal wrap nonumber norelativenumber
        \| noremap <buffer> <Esc> :<C-u>q<CR>
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

augroup AutoRead " {{{2
  autocmd!
  autocmd FocusGained,BufEnter * :checktime
augroup END

augroup IgnoreCaseCommandMode " {{{2
    autocmd!
    autocmd CmdLineEnter : set nosmartcase
    autocmd CmdLineLeave : set smartcase
augroup END

augroup FixSleuthPolyglot " {{{2
  autocmd!
  " Fix vim-sleuth and vim-polyglot not getting along
  autocmd Filetype * if &filetype != 'markdown' | call plug#load('vim-sleuth') | endif
augroup END


" }}}2


" }}}1
