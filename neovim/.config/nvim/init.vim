" PLUGINS {{{1
call plug#begin()

" Color schemes
Plug 'rakr/vim-one'
Plug 'bluz71/vim-moonfly-colors'
Plug 'Lokaltog/vim-monotone'
Plug 'evanrelf/papercolor-theme'

" Appearance
Plug 'itchyny/lightline.vim' | Plug 'maximbaz/lightline-ale'
Plug 'roman/golden-ratio', { 'on': ['GoldenRatioToggle'] }
Plug 'jeffkreeftmeijer/vim-numbertoggle'

" Movement
Plug 'jeetsukumaran/vim-indentwise'
Plug 'junegunn/vim-slash'
Plug 'critiqjo/husk-x.vim'
Plug 'andymass/vim-matchup'

" Editing
Plug 'machakann/vim-sandwich'
Plug 'tomtom/tcomment_vim'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'

" Completion
" Plug 'zxqfl/tabnine-vim'
Plug 'SirVer/ultisnips'
Plug 'tmsvg/pear-tree'
" Plug 'jiangmiao/auto-pairs'
Plug 'alvan/vim-closetag'
" Plug 'tpope/vim-endwise'

" Formatting
Plug 'sbdchd/neoformat', { 'on': ['Neoformat'] }
Plug 'junegunn/vim-easy-align'
Plug 'sickill/vim-pasta'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-sleuth'

" Information
Plug 'w0rp/ale'
Plug 'airblade/vim-gitgutter'
Plug 'simnalamburt/vim-mundo', { 'on': ['MundoToggle', 'MundoShow'] }
Plug 'tpope/vim-fugitive' | Plug 'tpope/vim-rhubarb' | Plug 'shumphrey/fugitive-gitlab.vim'

" Syntax
Plug 'sheerun/vim-polyglot'
Plug 'evanrelf/haskell-vim'
Plug 'vmchale/dhall-vim'

" Files
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'tpope/vim-eunuch'

" Miscellaneous
Plug 'majutsushi/tagbar', { 'on': ['TagbarToggle', 'TagbarOpen'] }
Plug 'moll/vim-bbye'
Plug 'tpope/vim-repeat'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'Konfekt/FastFold'

call plug#end()

" Plugin settings {{{2

" monotone
" let g:monotone_color = [30, 50, 80]

" one
let g:one_allow_italics = 1
set t_8b=^[[48;2;%lu;%lu;%lum
set t_8f=^[[38;2;%lu;%lu;%lum

" lightline
let g:lightline = {}
let g:lightline.colorscheme = 'one'
let g:lightline.component_expand = {
      \ 'linter_checking': 'lightline#ale#checking',
      \ 'linter_errors': 'lightline#ale#errors',
      \ 'linter_warnings': 'lightline#ale#warnings',
      \ 'linter_ok': 'lightline#ale#ok',
      \ }
let g:lightline.component_function = {
      \ 'gitbranch': 'fugitive#head'
      \ }
let g:lightline.component_type = {
      \ 'linter_checking': 'middle',
      \ 'linter_errors': 'error',
      \ 'linter_warnings': 'warning',
      \ 'linter_ok': 'middle',
      \ }
let g:lightline.active = {
      \ 'left': [[], ['paste', 'filename', 'readonly', 'modified'], ['linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok']],
      \ 'right': [[], ['filetype'], ['lineinfo']]
      \}
let g:lightline#ale#indicator_checking = '...'
let g:lightline#ale#indicator_ok = ''

" buftabline
let g:buftabline_show = 1
let g:buftabline_indicators = 1

" indentline
let g:indentLine_setColors = 0

" golden-ratio
let g:golden_ratio_autocommand = 0

" match up
let g:matchup_matchparen_status_offscreen = 0
let g:matchup_matchparen_deferred = 1

" ultisnips
let g:UltiSnipsExpandTrigger = '<Tab>'
" let g:UltiSnipsJumpForwardTrigger = '<C-j>'
" let g:UltiSnipsJumpBackwardTrigger = '<C-k>'
let g:UltiSnipsJumpForwardTrigger = '<Tab>'
let g:UltiSnipsJumpBackwardTrigger = '<S-Tab>'
let g:UltiSnipsEditSplit = 'vertical'

" pear tree
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1
let g:pear_tree_pairs = {
      \ '(': {'closer': ')'},
      \ '[': {'closer': ']'},
      \ '{': {'closer': '}'},
      \ "'": {'closer': "'"},
      \ '"': {'closer': '"'},
      \ '{-': {'closer': '-}'}
      \ }

" auto-pairs
let g:AutoPairsMultilineClose = 0

" neoformat
let g:neoformat_only_msg_on_error = 1

" ale
let g:ale_sign_column_always = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = { 'haskell': ['stack_build', 'hlint'] }
let g:ale_fixers = { 'elm': ['elm-format'] }
let g:ale_fix_on_save = 1
let g:ale_sign_error = '▌'
let g:ale_sign_warning = '▌'
let g:ale_echo_msg_info_str = '[INFO]'
let g:ale_echo_msg_error_str = '[ERR]'
let g:ale_echo_msg_warning_str = '[WARN]'
let g:ale_echo_msg_format = '%severity% %code: %%s [%linter%]'

" better-whitespace
let g:strip_whitelines_at_eof = 1
let g:strip_whitespace_on_save = 1
let g:strip_whitespace_confirm = 0
let g:strip_only_modified_lines = 1
let g:show_spaces_that_precede_tabs = 1

" polyglot
let g:polyglot_disabled = ['haskell']
let g:vim_markdown_conceal = 0
let g:vim_markdown_new_list_item_indent = 0
let g:elm_setup_keybindings = 0
let g:elm_format_autosave = 0
let g:haskell_backpack = 1
let g:haskell_indent_if = 2
let g:haskell_indent_in = 0
let g:purescript_indent_case = 2

" tagbar
let g:tagbar_compact = 1
let g:tagbar_sort = 0
let g:tagbar_type_elm = {
  \ 'kinds' : [
      \ 'm:modules:0:0',
      \ 'i:imports:1:0',
      \ 't:types:0:0',
      \ 'a:type aliases:0:0',
      \ 'c:type constructors:0:0',
      \ 'p:ports:0:0',
      \ 'f:functions:0:0',
      \ 's:functions:0:0',
  \ ]
  \}
let g:tagbar_type_haskell = {
  \ 'ctagsbin'    : 'hasktags',
  \ 'ctagsargs'   : '-x -c -o-',
  \ 'kinds'       : [
      \  'm:modules:0:1',
      \  'd:data:0:1',
      \  'd_gadt:data gadt:0:1',
      \  'nt:newtype:0:1',
      \  'c:classes:0:1',
      \  'i:instances:0:1',
      \  'cons:constructors:0:1',
      \  'c_gadt:constructor gadt:0:1',
      \  'c_a:constructor accessors:1:1',
      \  't:type names:0:1',
      \  'pt:pattern types:0:1',
      \  'pi:pattern implementations:0:1',
      \  'ft:function types:0:1',
      \  'fi:function implementations:0:1',
      \  'o:others:0:1'
  \ ],
  \ 'sro'          : '.',
  \ 'kind2scope'   : {
      \ 'm'        : 'module',
      \ 'd'        : 'data',
      \ 'd_gadt'   : 'd_gadt',
      \ 'c_gadt'   : 'c_gadt',
      \ 'nt'       : 'newtype',
      \ 'cons'     : 'cons',
      \ 'c_a'      : 'accessor',
      \ 'c'        : 'class',
      \ 'i'        : 'instance'
  \ },
  \ 'scope2kind'   : {
      \ 'module'   : 'm',
      \ 'data'     : 'd',
      \ 'newtype'  : 'nt',
      \ 'cons'     : 'c_a',
      \ 'd_gadt'   : 'c_gadt',
      \ 'class'    : 'ft',
      \ 'instance' : 'ft'
  \ }
  \ }

" nerdtree
let g:NERDTreeShowHidden = 1
let g:NERDTreeNaturalSort = 1
let g:NERDTreeRespectWildIgnore = 1
let g:NERDTreeMinimalUI = 1

" gitgutter
let g:gitgutter_map_keys = 0
let g:gitgutter_grep = 'rg'

" fugitive
let g:fugitive_gitlab_domains = ['https://gitlab.vetpro.us']

" }}}2

" SETTINGS {{{1
" Appearance {{{2
set termguicolors
colorscheme one
set background=dark
set cursorline
set colorcolumn=81
set number
set relativenumber
set rulerformat=%7(%3(%l%),%3(%c%V%)%)
set noshowmode
set shortmess=filmxTWIcF
set title
set splitbelow
set splitright
set scrolloff=2
set conceallevel=2
set concealcursor=nc

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
set formatoptions=cqnj

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

" Wild mode {{{2
set wildmode=longest:full,full
set wildignore+=*/.git/*,.DS_Store
set wildignorecase

" Miscellaneous {{{2
set mouse=a
set hidden
set virtualedit=block
set lazyredraw
set updatetime=250
set inccommand=nosplit
set keywordprg=:help
scriptencoding 'utf-8'

" }}}2


" COMMANDS {{{1
function! CommandCabbr(abbreviation, expansion) abort
  silent execute 'cabbrev ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ Command call CommandCabbr(<f-args>)
Command bd Bd

command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'
command! Bg let &background=(&background == 'dark' ? 'light' : 'dark')
command! Num let &number=(&number == 0 ? 1 : 0) | let &relativenumber=&number
command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'options': '--exact --delimiter : --nth 4..' },
      \   <bang>0)
command! -bang -nargs=* GRg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'dir': systemlist('git rev-parse --show-toplevel')[0],
      \     'options': '--exact --delimiter : --nth 4..' },
      \   <bang>0)


" MAPPINGS {{{1
noremap Y y$
noremap j gj
noremap k gk
nnoremap J m`J``
xnoremap < <gv
xnoremap > >gv
nnoremap > >>
nnoremap < <<
xnoremap p "_dP
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]
tnoremap <Esc> <C-\><C-n>
noremap Q @q
noremap <silent> g. :<C-u>ALENextWrap<CR>zz
noremap <silent> g, :<C-u>ALEPreviousWrap<CR>zz
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)
map <Plug>(slash-after) zz
noremap <Left> 5zh
noremap <Right> 5zl
noremap <Up> 5<C-y>
noremap <Down> 5<C-e>
noremap gh ^
noremap gj G
noremap gk gg
noremap gl g_
noremap <Backspace> :nohlsearch<CR>
noremap <silent> <Tab> :tabnext<CR>
noremap <silent> <S-Tab> :tabprev<CR>
noremap <C-]> <C-]>zz
noremap <C-t> <C-t>zz

" Leader
map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nnoremap <Leader>g :%g/
xnoremap <Leader>g :g/
nnoremap <Leader>n :%norm 0
xnoremap <Leader>n :norm 0
noremap <silent> <Leader>= :Neoformat<CR>
map <silent> <Leader>ad <Plug>(ale_detail)
noremap <silent> <Leader>f :<C-u>GFiles<CR>
noremap <silent> <Leader>F :<C-u>Files<CR>
noremap <silent> <Leader>r :<C-u>GRg<CR>
noremap <silent> <Leader>R :<C-u>Rg<CR>
noremap <silent> <Leader>b :<C-u>Buffers<CR>
xnoremap <Leader>S :sort<CR>
noremap <silent> <Leader>G <C-w>=:<C-u>GoldenRatioToggle<CR>
noremap <silent> <Leader>T :TagbarToggle<CR>
noremap <silent> <Leader>U :MundoToggle<CR>

" Available
noremap S <Nop>
noremap + <Nop>
noremap _ <Nop>
" noremap # <Nop>
xnoremap P <Nop>
xnoremap Z <Nop>


" AUTOCOMMANDS {{{1
augroup FileTypeSettings " {{{2
  autocmd!
  autocmd FileType elm
        \  setlocal softtabstop=4
        \| setlocal shiftwidth=4
  autocmd FileType fish
        \  setlocal softtabstop=4
        \| setlocal shiftwidth=4
  autocmd FileType cpp setlocal commentstring=//\ %s
  autocmd FileType gitcommit setlocal colorcolumn=73 spell
  autocmd BufEnter Dockerfile* setlocal filetype=Dockerfile
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd TermOpen * setlocal wrap nonumber norelativenumber
  autocmd FileType man
        \  setlocal laststatus=0 noruler wrap
        \| noremap <buffer> h 2zh
        \| noremap <buffer> j 2<C-e>L0
        \| noremap <buffer> k 2<C-y>H0
        \| noremap <buffer> l 2zl
        \| noremap <buffer> d <C-d>
        \| noremap <buffer> u <C-u>
        " \| autocmd! LazyLoadPlugins
  autocmd FileType vim-plug
        \  setlocal wrap nonumber norelativenumber
        \| nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType fzf
        \  nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType ale-preview
        \  setlocal wrap nonumber norelativenumber
        \| nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType markdown,text,latex,tex setlocal wrap nonumber norelativenumber
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
  autocmd CmdLineEnter : setlocal nosmartcase
  autocmd CmdLineLeave : setlocal smartcase
augroup END

augroup RedrawOnResize " {{{2
  autocmd!
  autocmd VimResized * redraw!
augroup END

augroup Concealing " {{{2
  autocmd!
  autocmd FileType haskell,purescript
        \  syntax match Conceal "\<forall\>" conceal cchar=∀
        \| hi! link Conceal Operator
augroup END

" augroup ColorSchemeTweaks " {{{2
"   autocmd!
"   autocmd VimEnter *
"         \  highlight! Normal ctermfg=251 ctermbg=0 guifg=#c6c6c6 guibg=#000000
"         \| highlight! default link ExtraWhitespace DiffDelete
"         \| highlight! default link BufTabLineCurrent WildMenu
"         \| highlight! default link BufTabLineActive CursorLineNr
"         \| highlight! default link BufTabLineHidden StatusLineNC
"         \| highlight! default link BufTabLineFill StatusLineNC
"         \| highlight! Normal ctermbg=NONE guibg=NONE
"         \| highlight! NonText ctermbg=NONE guibg=NONE
"       " \| highlight! default link StatusLine MatchParen
"       " \| highlight! default link StatusLineNC Normal
" augroup END

" augroup OpenTagBar " {{{2
"   autocmd!
"   autocmd VimEnter *.elm TagbarOpen
" augroup END

" augroup CloseNERDTree " {{{
"   autocmd!
"   autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | quit | endif
" augroup END

" augroup LazyLoadPlugins " {{{2
"   autocmd!
"   autocmd CursorHold,CursorHoldI *
"         \  call plug#load('tabnine-vim')
"         \| autocmd! LazyLoadPlugins
" augroup END

" }}}2

" vim: foldenable foldmethod=marker
