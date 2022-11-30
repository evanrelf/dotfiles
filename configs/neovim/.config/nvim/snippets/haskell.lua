local luasnip = require("luasnip")
local s = luasnip.s
local i = luasnip.insert_node
local f = luasnip.function_node
local fmt = require("luasnip.extras.fmt").fmt

local same = function(index)
  return f(function(args)
    return args[1]
  end, { index })
end

local auto = function(regex, nodes)
  local trigger = { trig = regex, regTrig = true }
  local options = {
    show_condition = function()
      return false
    end,
  }
  return s(trigger, nodes, options)
end

local snippets = nil

local autosnippets = {
  auto("^lang ", fmt("{{-# LANGUAGE {} #-}}", { i(0) })),
  auto("^opt ", fmt("{{-# OPTIONS_GHC {} #-}}", { i(0) })),
  auto("^module ", fmt("module {} where", { i(0) })),
}

return snippets, autosnippets
