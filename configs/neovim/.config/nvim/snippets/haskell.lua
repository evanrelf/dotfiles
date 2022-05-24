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
  auto("^i ", fmt("import {}", { i(0) })),
  auto("^ii ", fmt("import qualified {}", { i(0) })),
  auto("^uio ", fmt("import qualified UnliftIO.{} as {}", { same(1), i(1) })),
  auto("^class ", fmt("class {} where", { i(0) })),
  auto("^instance ", fmt("instance {} where", { i(0) })),
  auto("^type family ", fmt("type family {} where", { i(0) })),
}

return snippets, autosnippets
