-- luacheck: globals hs

local wm = require "wm"
local run = require "run"

local bind = hs.hotkey.bind
local meh = {"cmd", "alt", "ctrl"}
local hyper = {"shift", "cmd", "alt", "ctrl"}

-- -- Auto-fullscreen Spotify on launch
-- hs.application.watcher.new(function(appName, eventType, app)
--   if appName == "Spotify" and eventType == hs.application.watcher.launched then
--     local window = app:mainWindow()
--     hs.timer.doAfter(2, function()
--       window:setFullScreen(not window:isFullScreen())
--     end)
--   end
-- end):start()

-- Floating windows (Hammerspoon)
bind(meh, "\\", hs.reload)
bind(meh, "delete", function() hs.eventtap.keyStroke({}, "forwarddelete") end)
bind(meh, "R", wm.halfLeft)
bind(meh, "P", wm.halfCenter)
bind(meh, "T", wm.halfRight)
bind(meh, "1", wm.thirdLeft)
bind(meh, "2", wm.thirdCenter)
bind(meh, "3", wm.thirdRight)
bind(meh, "4", wm.twoThirdsLeft)
bind(meh, "B", wm.twoThirdsCenter)
bind(meh, "5", wm.twoThirdsRight)
bind(meh, "Q", wm.quarterTopLeft)
bind(meh, "W", wm.quarterTopRight)
bind(meh, "A", wm.quarterBottomLeft)
bind(meh, "S", wm.quarterBottomRight)
bind(meh, "F", wm.full)
bind(meh, "C", wm.center)
bind(meh, "Z", wm.undo)
bind(meh, "Y", wm.redo)
bind(meh, "I", wm.save)
bind(meh, "O", wm.load)
bind(meh, "tab", wm.nextScreen)

-- Tiling windows (Amethyst)
bind({"alt", "shift"}, "return", run.terminal)

-- tell application "iTerm"
-- 	create window with default profile
-- end tell

hs.alert.show("Hammerspoon loaded!")
