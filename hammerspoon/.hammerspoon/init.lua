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
bind(hyper, "F", wm.full)
bind(hyper, "C", wm.center)
bind(hyper, "Z", wm.undo)
bind(hyper, "Y", wm.redo)

-- -- Top left 1/4
-- w(hyper, "Q", { x = 0, y = 0, w = (1/2), h = (1/2) })
-- -- Top right 1/4
-- w(hyper, "W", { x = (1/2), y = 0, w = (1/2), h = (1/2) })
-- -- Bottom left 1/4
-- w(hyper, "A", { x = 0, y = (1/2), w = (1/2), h = (1/2) })
-- -- Bottom right 1/4
-- w(hyper, "S", { x = (1/2), y = (1/2), w = (1/2), h = (1/2) })

hs.alert.show("Hammerspoon loaded!")
