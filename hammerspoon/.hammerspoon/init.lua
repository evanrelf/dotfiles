-- luacheck: globals hs

local wm = require "wm"

local bind = hs.hotkey.bind
local hyper = {"shift", "cmd", "alt", "ctrl"}

-- Auto-fullscreen Spotify on launch
hs.application.watcher.new(function(appName, eventType, app)
  if appName == "Spotify" and eventType == hs.application.watcher.launched then
    local window = app:mainWindow()
    hs.timer.doAfter(2, function()
      window:setFullScreen(not window:isFullScreen())
    end)
  end
end):start()

-- hs.noises.new(hs.alert.show):start()

bind(hyper, "\\", hs.reload)
bind(hyper, "delete", function() hs.eventtap.keyStroke({}, "forwarddelete") end)
bind(hyper, "R", wm.halfLeft)
bind(hyper, "P", wm.halfCenter)
bind(hyper, "T", wm.halfRight)
bind(hyper, "1", wm.thirdLeft)
bind(hyper, "2", wm.thirdCenter)
bind(hyper, "3", wm.thirdRight)
bind(hyper, "4", wm.twoThirdsLeft)
bind(hyper, "B", wm.twoThirdsCenter)
bind(hyper, "5", wm.twoThirdsRight)
bind(hyper, "Q", wm.quarterTopLeft)
bind(hyper, "W", wm.quarterTopRight)
bind(hyper, "A", wm.quarterBottomLeft)
bind(hyper, "S", wm.quarterBottomRight)
bind(hyper, "F", wm.full)
bind(hyper, "C", wm.center)
bind(hyper, "Z", wm.undo)
bind(hyper, "Y", wm.redo)
bind(hyper, "I", wm.save)
bind(hyper, "O", wm.load)
bind(hyper, "Tab", wm.nextScreen)

hs.alert.show("Hammerspoon loaded!")
