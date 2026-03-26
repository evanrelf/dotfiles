-- https://stephango.com/flexoki

local M = {}

local white = "#FFFFFF"
local paper = "#FFFCF0"
local base_50 = "#F2F0E5"
local base_100 = "#E6E4D9"
local base_150 = "#DAD8CE"
local base_200 = "#CECDC3"
local base_300 = "#B7B5AC"
local base_400 = "#9F9D96"
local base_500 = "#878580"
local base_600 = "#6F6E69"
local base_700 = "#575653"
local base_800 = "#403E3C"
local base_850 = "#343331"
local base_900 = "#282726"
local base_950 = "#1C1B1A"
local black = "#100F0F"

local red_50 = "#FFE1D5"
local red_100 = "#FFCABB"
local red_150 = "#FDB2A2"
local red_200 = "#F89A8A"
local red_300 = "#E8705F"
local red_400 = "#D14D41"
local red_500 = "#C03E35"
local red_600 = "#AF3029"
local red_700 = "#942822"
local red_800 = "#6C201C"
local red_850 = "#551B18"
local red_900 = "#3E1715"
local red_950 = "#261312"

local orange_50 = "#FFE7CE"
local orange_100 = "#FED3AF"
local orange_150 = "#FCC192"
local orange_200 = "#F9AE77"
local orange_300 = "#EC8B49"
local orange_400 = "#DA702C"
local orange_500 = "#CB6120"
local orange_600 = "#BC5215"
local orange_700 = "#9D4310"
local orange_800 = "#71320D"
local orange_850 = "#59290D"
local orange_900 = "#40200D"
local orange_950 = "#27180E"

local yellow_50 = "#FAEEC6"
local yellow_100 = "#F6E2A0"
local yellow_150 = "#F1D67E"
local yellow_200 = "#ECCB60"
local yellow_300 = "#DFB431"
local yellow_400 = "#D0A215"
local yellow_500 = "#BE9207"
local yellow_600 = "#AD8301"
local yellow_700 = "#8E6B01"
local yellow_800 = "#664D01"
local yellow_850 = "#503D02"
local yellow_900 = "#3A2D04"
local yellow_950 = "#241E08"

local green_50 = "#EDEECF"
local green_100 = "#DDE2B2"
local green_150 = "#CDD597"
local green_200 = "#BEC97E"
local green_300 = "#A0AF54"
local green_400 = "#879A39"
local green_500 = "#768D21"
local green_600 = "#66800B"
local green_700 = "#536907"
local green_800 = "#3D4C07"
local green_850 = "#313D07"
local green_900 = "#252D09"
local green_950 = "#1A1E0C"

local cyan_50 = "#DDF1E4"
local cyan_100 = "#BFE8D9"
local cyan_150 = "#A2DECE"
local cyan_200 = "#87D3C3"
local cyan_300 = "#5ABDAC"
local cyan_400 = "#3AA99F"
local cyan_500 = "#2F968D"
local cyan_600 = "#24837B"
local cyan_700 = "#1C6C66"
local cyan_800 = "#164F4A"
local cyan_850 = "#143F3C"
local cyan_900 = "#122F2C"
local cyan_950 = "#101F1D"

local blue_50 = "#E1ECEB"
local blue_100 = "#C6DDE8"
local blue_150 = "#ABCFE2"
local blue_200 = "#92BFDB"
local blue_300 = "#66A0C8"
local blue_400 = "#4385BE"
local blue_500 = "#3171B2"
local blue_600 = "#205EA6"
local blue_700 = "#1A4F8C"
local blue_800 = "#163B66"
local blue_850 = "#133051"
local blue_900 = "#12253B"
local blue_950 = "#101A24"

local purple_50 = "#F0EAEC"
local purple_100 = "#E2D9E9"
local purple_150 = "#D3CAE6"
local purple_200 = "#C4B9E0"
local purple_300 = "#A699D0"
local purple_400 = "#8B7EC8"
local purple_500 = "#735EB5"
local purple_600 = "#5E409D"
local purple_700 = "#4F3685"
local purple_800 = "#3C2A62"
local purple_850 = "#31234E"
local purple_900 = "#261C39"
local purple_950 = "#1A1623"

local magenta_50 = "#FEE4E5"
local magenta_100 = "#FCCFDA"
local magenta_150 = "#F9B9CF"
local magenta_200 = "#F4A4C2"
local magenta_300 = "#E47DA8"
local magenta_400 = "#CE5D97"
local magenta_500 = "#B74583"
local magenta_600 = "#A02F6F"
local magenta_700 = "#87285E"
local magenta_800 = "#641F46"
local magenta_850 = "#4F1B39"
local magenta_900 = "#39172B"
local magenta_950 = "#24131D"

-- Use this as a reference:
-- https://github.com/neovim/neovim/blob/master/runtime/colors/vim.lua

vim.cmd('highlight clear')
vim.g.colors_name = 'flexoki'

local hi = function(name, value)
  value.force = true
  value.cterm = value.cterm or {}
  vim.api.nvim_set_hl(0, name, value)
end

hi('Normal', {})

if vim.o.background == 'light' then
else
end

return M
