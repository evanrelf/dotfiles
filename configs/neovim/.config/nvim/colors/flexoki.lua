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

-- Semantic color aliases
local tx, tx_2, tx_3
local ui, ui_2, ui_3
local bg, bg_2
local red, orange, yellow, green, cyan, blue, purple, magenta
local red_bg, orange_bg, yellow_bg, green_bg, cyan_bg, blue_bg, purple_bg

if vim.o.background == 'light' then
  tx = black
  tx_2 = base_600
  tx_3 = base_300
  ui = base_100
  ui_2 = base_150
  ui_3 = base_200
  bg = paper
  bg_2 = base_50
  red = red_600
  orange = orange_600
  yellow = yellow_600
  green = green_600
  cyan = cyan_600
  blue = blue_600
  purple = purple_600
  magenta = magenta_600
  red_bg = red_100
  orange_bg = orange_100
  yellow_bg = yellow_100
  green_bg = green_100
  cyan_bg = cyan_100
  blue_bg = blue_100
  purple_bg = purple_100
else
  tx = base_200
  tx_2 = base_500
  tx_3 = base_700
  ui = base_900
  ui_2 = base_850
  ui_3 = base_800
  bg = black
  bg_2 = base_950
  red = red_400
  orange = orange_400
  yellow = yellow_400
  green = green_400
  cyan = cyan_400
  blue = blue_400
  purple = purple_400
  magenta = magenta_400
  red_bg = red_900
  orange_bg = orange_900
  yellow_bg = yellow_900
  green_bg = green_900
  cyan_bg = cyan_900
  blue_bg = blue_900
  purple_bg = purple_900
end

--stylua: ignore start

-- General
hi('Normal',       { fg = tx, bg = bg })
hi('NormalFloat',  { fg = tx, bg = ui })
hi('FloatBorder',  { fg = tx_3, bg = ui })
hi('FloatTitle',   { link = 'Title' })
hi('FloatFooter',  { link = 'Title' })
hi('Conceal',      { fg = tx_3 })
hi('Cursor',       { fg = bg, bg = blue })
hi('lCursor',      { link = 'Cursor' })
hi('TermCursor',   { reverse = true })
hi('ColorColumn',  { bg = ui })
hi('CursorColumn', { bg = bg_2 })
hi('CursorLine',   { bg = bg_2 })
hi('Visual',       { bg = blue_bg })
hi('VisualNOS',    { link = 'Visual' })
hi('Search',       { bg = yellow_bg })
hi('IncSearch',    { fg = bg, bg = yellow })
hi('CurSearch',    { fg = bg, bg = blue })
hi('Substitute',   { link = 'Search' })
hi('MatchParen',   { bg = yellow_bg })
hi('ModeMsg',      { bold = true })
hi('MoreMsg',      { fg = green, bold = true })
hi('Question',     { fg = green, bold = true })
hi('ErrorMsg',     { fg = red })
hi('WarningMsg',   { fg = orange })
hi('Directory',    { fg = blue })
hi('Title',        { bold = true })
hi('NonText',      { fg = tx_3 })
hi('EndOfBuffer',  { fg = tx_3 })
hi('Whitespace',   { fg = ui_3 })
hi('SpecialKey',   { fg = ui_3 })
hi('Folded',       { fg = tx_2, bg = bg_2 })
hi('FoldColumn',   { fg = tx_3 })
hi('SignColumn',   { fg = tx_3 })
hi('WildMenu',     { fg = bg, bg = blue })

-- Status line and tab line
hi('StatusLine',   { fg = tx, bg = ui })
hi('StatusLineNC', { fg = tx_2, bg = ui })
hi('WinBar',       { bold = true })
hi('WinBarNC',     { link = 'WinBar' })
hi('WinSeparator', { fg = ui_3 })
hi('VertSplit',    { link = 'WinSeparator' })
hi('TabLine',      { fg = tx_2, bg = ui })
hi('TabLineFill',  { bg = ui })
hi('TabLineSel',   { fg = tx, bg = bg, bold = true })
hi('MsgSeparator', { link = 'StatusLine' })

-- Popup menu
hi('Pmenu',         { fg = tx, bg = ui })
hi('PmenuSel',      { fg = bg, bg = blue })
hi('PmenuSbar',     { bg = ui_2 })
hi('PmenuThumb',    { bg = tx_3 })
hi('PmenuKind',     { link = 'Pmenu' })
hi('PmenuKindSel',  { link = 'PmenuSel' })
hi('PmenuMatch',    { link = 'Pmenu' })
hi('PmenuMatchSel', { link = 'PmenuSel' })
hi('PmenuExtra',    { link = 'Pmenu' })
hi('PmenuExtraSel', { link = 'PmenuSel' })

-- Line numbers
hi('LineNr',         { fg = tx_3 })
hi('CursorLineNr',  { fg = tx_2, bold = true })
hi('LineNrAbove',    { link = 'LineNr' })
hi('LineNrBelow',    { link = 'LineNr' })
hi('CursorLineSign', { link = 'SignColumn' })
hi('CursorLineFold', { link = 'FoldColumn' })
hi('QuickFixLine',   { link = 'Search' })

-- Diff
hi('DiffAdd',    { bg = green_bg })
hi('DiffChange', { bg = blue_bg })
hi('DiffDelete', { bg = red_bg })
hi('DiffText',   { bg = blue_bg, bold = true })
hi('Added',      { fg = green })
hi('Changed',    { fg = blue })
hi('Removed',    { fg = red })

-- Spell
hi('SpellBad',   { sp = red, undercurl = true })
hi('SpellCap',   { sp = blue, undercurl = true })
hi('SpellLocal', { sp = cyan, undercurl = true })
hi('SpellRare',  { sp = purple, undercurl = true })

-- Syntax (minimal: only comments, strings, and constants get color)
hi('Comment',    { fg = tx_2 })
hi('Constant',   { fg = blue })
hi('String',     { fg = yellow })
hi('Character',  { link = 'String' })
hi('Number',     { link = 'Constant' })
hi('Boolean',    { link = 'Constant' })
hi('Float',      { link = 'Number' })
hi('Identifier', {})
hi('Function',   {})
hi('Statement',  {})
hi('Conditional', { link = 'Statement' })
hi('Repeat',      { link = 'Statement' })
hi('Label',       { link = 'Statement' })
hi('Operator',   {})
hi('Keyword',    { link = 'Statement' })
hi('Exception',  { link = 'Statement' })
hi('PreProc',    {})
hi('Include',    { link = 'PreProc' })
hi('Define',     { link = 'PreProc' })
hi('Macro',      { link = 'PreProc' })
hi('PreCondit',  { link = 'PreProc' })
hi('Type',       {})
hi('StorageClass', { link = 'Type' })
hi('Structure',    { link = 'Type' })
hi('Typedef',      { link = 'Type' })
hi('Special',    {})
hi('SpecialChar',    { link = 'Special' })
hi('Tag',            { link = 'Special' })
hi('Delimiter',      { link = 'Special' })
hi('SpecialComment', { link = 'Special' })
hi('Debug',          { link = 'Special' })
hi('Error',      { bg = red_bg })
hi('Todo',       { fg = tx, bg = yellow_bg, bold = true })
hi('Underlined', { fg = blue, underline = true })
hi('Ignore',     {})

-- Diagnostics
hi('DiagnosticError', { fg = red })
hi('DiagnosticWarn',  { fg = orange })
hi('DiagnosticInfo',  { fg = blue })
hi('DiagnosticHint',  { fg = tx_3 })
hi('DiagnosticOk',    { fg = green })

hi('DiagnosticUnderlineError', { sp = red, undercurl = true })
hi('DiagnosticUnderlineWarn',  { sp = orange, undercurl = true })
hi('DiagnosticUnderlineInfo',  { sp = blue, undercurl = true })
hi('DiagnosticUnderlineHint',  { sp = tx_3, undercurl = true })
hi('DiagnosticUnderlineOk',    { sp = green, undercurl = true })

hi('DiagnosticVirtualTextError', { fg = red, bg = red_bg })
hi('DiagnosticVirtualTextWarn',  { fg = orange, bg = orange_bg })
hi('DiagnosticVirtualTextInfo',  { fg = blue, bg = blue_bg })
hi('DiagnosticVirtualTextHint',  { fg = tx_3, bg = bg_2 })
hi('DiagnosticVirtualTextOk',    { fg = green, bg = green_bg })

hi('DiagnosticFloatingError', { link = 'DiagnosticError' })
hi('DiagnosticFloatingWarn',  { link = 'DiagnosticWarn' })
hi('DiagnosticFloatingInfo',  { link = 'DiagnosticInfo' })
hi('DiagnosticFloatingHint',  { link = 'DiagnosticHint' })
hi('DiagnosticFloatingOk',    { link = 'DiagnosticOk' })

hi('DiagnosticSignError', { link = 'DiagnosticError' })
hi('DiagnosticSignWarn',  { link = 'DiagnosticWarn' })
hi('DiagnosticSignInfo',  { link = 'DiagnosticInfo' })
hi('DiagnosticSignHint',  { link = 'DiagnosticHint' })
hi('DiagnosticSignOk',    { link = 'DiagnosticOk' })

hi('DiagnosticDeprecated',  { sp = tx_3, strikethrough = true })
hi('DiagnosticUnnecessary', { link = 'Comment' })

-- LSP
hi('LspInlayHint',      { fg = tx_3, bg = bg_2 })
hi('LspReferenceText',  { bg = bg_2 })
hi('LspReferenceRead',  { bg = bg_2 })
hi('LspReferenceWrite', { bg = bg_2 })

-- Snippets
hi('SnippetTabstop',       { link = 'Visual' })
hi('SnippetTabstopActive', { link = 'SnippetTabstop' })

-- Misc
hi('FloatShadow',          { bg = black, blend = 80 })
hi('FloatShadowThrough',   { bg = black, blend = 100 })
hi('RedrawDebugNormal',    { reverse = true })
hi('RedrawDebugClear',     { bg = yellow_400 })
hi('RedrawDebugComposed',  { bg = green_400 })
hi('RedrawDebugRecompose', { bg = red_400 })

-- Treesitter
hi('@comment',     { link = 'Comment' })
hi('@comment.todo', { link = 'Todo' })
hi('@punctuation', { link = 'Delimiter' })

hi('@constant',          {})
hi('@constant.builtin',  {})
hi('@constant.macro',    {})
hi('@string',            { link = 'String' })
hi('@string.escape',     { link = 'String' })
hi('@string.special',    {})
hi('@variable.string',   { fg = blue })
hi('@character',         { link = 'String' })
hi('@character.special', { link = 'String' })
hi('@number',            { link = 'Constant' })
hi('@boolean',           { link = 'Constant' })
hi('@number.float',      { link = 'Constant' })

hi('@function',                   { link = 'Function' })
hi('@function.builtin',           { link = 'Function' })
hi('@function.macro',             { link = 'Function' })
hi('@function.method',            { link = 'Function' })
hi('@variable',                   { link = 'Identifier' })
hi('@variable.parameter',         { link = 'Identifier' })
hi('@variable.parameter.builtin', { link = 'Identifier' })
hi('@variable.member',            { link = 'Identifier' })
hi('@property',                   { link = 'Identifier' })
hi('@attribute',                  { link = 'Special' })
hi('@attribute.builtin',          { link = 'Special' })
hi('@constructor',                { link = 'Function' })

hi('@keyword',             { link = 'Keyword' })
hi('@keyword.conditional', { link = 'Keyword' })
hi('@keyword.repeat',      { link = 'Keyword' })
hi('@keyword.type',        { link = 'Keyword' })
hi('@keyword.exception',   { link = 'Keyword' })
hi('@keyword.import',      { link = 'Keyword' })
hi('@keyword.directive',   { link = 'PreProc' })
hi('@keyword.debug',       { link = 'Debug' })
hi('@label',               { link = 'Label' })
hi('@operator',            { link = 'Operator' })
hi('@type',                { link = 'Type' })
hi('@type.definition',     { link = 'Type' })
hi('@module',              { link = 'Identifier' })
hi('@tag',                 { link = 'Tag' })
hi('@tag.builtin',         { link = 'Tag' })

-- Treesitter markup
hi('@markup.heading',   { link = 'Title' })
hi('@markup.raw',       { fg = yellow })
hi('@markup.link',      { fg = blue, underline = true })
hi('@markup.link.url',  { link = 'Underlined' })
hi('@markup.underline', { underline = true })
hi('@markup.strong',    { bold = true })
hi('@markup.italic',    { italic = true })

-- LSP semantic tokens
hi('@lsp.type.class',         { link = 'Type' })
hi('@lsp.type.comment',       { link = 'Comment' })
hi('@lsp.type.decorator',     { link = 'Function' })
hi('@lsp.type.enum',          { link = 'Type' })
hi('@lsp.type.enumMember',    { link = 'Constant' })
hi('@lsp.type.function',      { link = 'Function' })
hi('@lsp.type.interface',     { link = 'Type' })
hi('@lsp.type.macro',         { link = 'Function' })
hi('@lsp.type.method',        { link = 'Function' })
hi('@lsp.type.namespace',     { link = 'Identifier' })
hi('@lsp.type.parameter',     { link = 'Identifier' })
hi('@lsp.type.property',      { link = 'Identifier' })
hi('@lsp.type.struct',        { link = 'Type' })
hi('@lsp.type.type',          { link = 'Type' })
hi('@lsp.type.typeParameter', { link = 'Type' })
hi('@lsp.type.variable',      { link = 'Identifier' })

--stylua: ignore end

return M
