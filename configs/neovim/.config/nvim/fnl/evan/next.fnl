(import-macros {: set!} :evan.macros)

(local paq (require :paq))

(paq.register "ishan9299/modus-theme-vim")
(set! termguicolors)
(set! background "light")
(set vim.g.modus_dim_inactive_window false)
(vim.cmd.colorscheme "modus-operandi")

(set! expandtab)
(set! shiftwidth 2)
(set! number)
(set! relativenumber)
(set! colorcolumn "81")
(set! noshowmode)
(set! ignorecase)
(set! smartcase)
(set! gdefault)
(set! nowrap)
(set! wildmode "longest:full,full")
(set! noswapfile)
(set! splitright)
(set! splitbelow)

(when vim.o.diff
  (set! nonumber)
  (set! norelativenumber))
