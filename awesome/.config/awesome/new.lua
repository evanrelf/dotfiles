local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

-- -- Standard awesome library
-- require("awful.autofocus")
-- -- Theme handling library
-- local beautiful = require("beautiful")
-- -- Notification library
-- local naughty = require("naughty")
-- local menubar = require("menubar")
-- local hotkeys_popup = require("awful.hotkeys_popup").widget

-- config
terminal = "st"
modKey = "Mod4"

-- layouts
awful.layout.layouts = {
  awful.layout.tile,
  awful.layout.max.fullscreen
}

-- bar
awful.screen.connect_for_each_screen(function(screen)
  awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, screen, awful.layout.layouts[1])
  screen.tagList = awful.widget.taglist(screen, awful.widget.taglist.filter.all, taglist_buttons)
  screen.wibox = awful.wibar({ position = "top", screen = screen })
  screen.wibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      screen.tagList
    },
    -- Middle widgets
    {},
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
    }
  }
end)

-- mouse
root.buttons(gears.table.join(
  awful.button({}, 3, function() mymainmenu:toggle() end),
))
clientbuttons = gears.table.join(
  awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
  awful.button({ modkey }, 1, awful.mouse.client.move),
  awful.button({ modkey }, 3, awful.mouse.client.resize))

-- keys
globalkeys = gears.table.join(
  awful.key({ modKey }, "s", hotkeys_popup.show_help,
    { description = "show help", group = "awesome" }),

  awful.key({ modKey }, "p", awful.tag.viewprev,
    { description = "view previous", group = "tag" }),

  awful.key({ modKey }, "n", awful.tag.viewnext,
    { description = "view next", group = "tag" }),

  awful.key({ modKey }, "Tab", awful.tag.history.restore,
    { description = "go back", group = "tag" }),

  awful.key({ modKey }, "j", function() awful.client.focus.byidx(1) end,
    { description = "focus next by index", group = "client" }),

  awful.key({ modKey }, "k", function() awful.client.focus.byidx(-1) end,
    { description = "focus previous by index", group = "client" }),

  awful.key({ modKey, "Shift" }, "j", function() awful.client.swap.byidx(1) end,
    { description = "swap with next client by index", group = "client" }),

  awful.key({ modKey, "Shift" }, "k", function() awful.client.swap.byidx(-1) end,
    { description = "swap with previous client by index", group = "client" }),

  awful.key({ modKey }, "l", function() awful.tag.incmwfact(0.05) end,
    { description = "increase master width factor", group = "layout" }),

  awful.key({ modKey }, "h", function() awful.tag.incmwfact(-0.05) end,
    { description = "decrease master width factor", group = "layout" }),

  -- awful.key({ modKey, "Control" }, "j", function() awful.screen.focus_relative(1) end,
  --   { description = "focus the next screen", group = "screen" }),

  -- awful.key({ modKey, "Control" }, "k", function() awful.screen.focus_relative(-1) end,
  --   { description = "focus the previous screen", group = "screen" }),

  -- awful.key({ modKey }, "u", awful.client.urgent.jumpto,
  --   { description = "jump to urgent client", group = "client" }),

  -- awful.key({ modKey }, "Tab",
  --   function()
  --     awful.client.focus.history.previous()
  --     if client.focus then
  --       client.focus:raise()
  --     end
  --   end,
  --   { description = "go back", group = "client" }),

  awful.key({ modKey, "Shift" }, "Return", function() awful.spawn(terminal) end,
    { description = "open a terminal", group = "launcher" }),

  awful.key({ modKey }, "q", awesome.restart,
    { description = "reload awesome", group = "awesome" }),

  awful.key({ modKey, "Shift" }, "q", awesome.quit,
    { description = "quit awesome", group = "awesome" }),

  awful.key({ modKey }, ".", function() awful.tag.incnmaster( 1, nil, true) end,
    { description = "increase the number of master clients", group = "layout" }),

  awful.key({ modKey }, ",", function() awful.tag.incnmaster(-1, nil, true) end,
    { description = "decrease the number of master clients", group = "layout" }),

  -- awful.key({ modKey, "Control" }, "h", function() awful.tag.incncol(1, nil, true) end,
  --   { description = "increase the number of columns", group = "layout" }),

  -- awful.key({ modKey, "Control" }, "l", function() awful.tag.incncol(-1, nil, true) end,
  --   { description = "decrease the number of columns", group = "layout" }),

  awful.key({ modKey }, "space", function() awful.layout.inc(1) end,
    { description = "select next", group = "layout" }),

  awful.key({ modKey, "Shift" }, "space", function() awful.layout.inc(-1) end,
    { description = "select previous", group = "layout" }),

  -- awful.key({ modKey, "Control" }, "n",
  --   function()
  --     local c = awful.client.restore()
  --     -- Focus restored client
  --     if c then
  --       client.focus = c
  --       c:raise()
  --     end
  --   end,
  --   {description = "restore minimized", group = "client"}),

  -- awful.key({ modKey }, "r", function() awful.screen.focused().mypromptbox:run() end,
  --   { description = "run prompt", group = "launcher" }),

  -- awful.key({ modKey }, "x",
  --   function()
  --     awful.prompt.run {
  --       prompt       = "Run Lua code: ",
  --       textbox      = awful.screen.focused().mypromptbox.widget,
  --       exe_callback = awful.util.eval,
  --       history_path = awful.util.get_cache_dir() .. "/history_eval"
  --     }
  --   end,
  --   { description = "lua execute prompt", group = "awesome" }),

  -- awful.key({ modKey }, "p", function() menubar.show() end,
  --   { description = "show the menubar", group = "launcher" })
)

clientkeys = gears.table.join(
  awful.key({ modkey,           }, "f",
    function (c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end,
    {description = "toggle fullscreen", group = "client"}),
  awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
    {description = "close", group = "client"}),
  awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
    {description = "toggle floating", group = "client"}),
  awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
    {description = "move to master", group = "client"}),
  awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
    {description = "move to screen", group = "client"}),
  awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
    {description = "toggle keep on top", group = "client"}),
  awful.key({ modkey,           }, "n",
    function (c)
      -- The client currently has the input focus, so it cannot be
      -- minimized, since minimized clients can't have the focus.
      c.minimized = true
    end ,
    {description = "minimize", group = "client"}),
  awful.key({ modkey,           }, "m",
    function (c)
      c.maximized = not c.maximized
      c:raise()
    end ,
    {description = "(un)maximize", group = "client"}),
  awful.key({ modkey, "Control" }, "m",
    function (c)
      c.maximized_vertical = not c.maximized_vertical
      c:raise()
    end ,
    {description = "(un)maximize vertically", group = "client"}),
  awful.key({ modkey, "Shift"   }, "m",
    function (c)
      c.maximized_horizontal = not c.maximized_horizontal
      c:raise()
    end ,
    {description = "(un)maximize horizontally", group = "client"})
)

for i = 1, 9 do
  globalkeys = gears.table.join(globalkeys,
    -- View tag only
    awful.key({ modkey }, "#" .. i + 9,
      function()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          tag:view_only()
        end
      end,
      { description = "view tag #"..i, group = "tag" }),
    -- Toggle tag display
    awful.key({ modkey, "Control" }, "#" .. i + 9,
      function()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          awful.tag.viewtoggle(tag)
        end
      end,
      { description = "toggle tag #" .. i, group = "tag" }),
    -- Move client to tag
    awful.key({ modkey, "Shift" }, "#" .. i + 9,
      function()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
      { description = "move focused client to tag #"..i, group = "tag" }),
    -- Toggle tag on focused client
    awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
      function()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:toggle_tag(tag)
          end
        end
      end,
      { description = "toggle focused client on tag #" .. i, group = "tag" })
  )
end

root.keys(globalkeys)

awful.rules.rules = {
  -- All clients
  { rule = {},
    properties = {
      border_width = beautiful.border_width,
      border_color = beautiful.border_normal,
      focus = awful.client.focus.filter,
      raise = true,
      keys = clientkeys,
      buttons = clientbuttons,
      screen = awful.screen.preferred,
      placement = awful.placement.no_overlap+awful.placement.no_offscreen
    }
  },
  -- Add decorations
  { rule_any = { type = { "normal", "dialog" } },
    properties = { titlebars_enabled = true }
  }
  { rule = { class = "[Ss]potify" },
    properties = { screen = 1, tag = "9" }
  }
}
