-- Global options
vim.o.colorcolumn = "81"
vim.o.showmode = false
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.wildmode = "longest:full,list:full"
vim.o.mouse = "a"
vim.o.shiftwidth = 2
vim.o.shiftround = true
vim.o.joinspaces = false
vim.o.ignorecase = true
vim.o.smartcase = true
vim.cmd "set wildignore+=*/.git/*,*/dist/*,*/dist-newstyle/*"
vim.o.wildignorecase = true
vim.o.gdefault = true
vim.o.swapfile = false
vim.o.writebackup = false
vim.o.hidden = true
vim.o.inccommand = "nosplit"
vim.o.virtualedit = "block,onemore"

-- Window options
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.wrap = false

-- Buffer options
vim.bo.expandtab = true