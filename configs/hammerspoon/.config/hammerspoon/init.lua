-- TODO: Use hs.window.filter

hs.window.animationDuration = 0

local fn = { "ctrl", "alt", "cmd" }
local sfn = { "shift", "ctrl", "alt", "cmd" }

--------------------------------------------------------------------------------
-- APP SWITCHING
--------------------------------------------------------------------------------

function focusApp(name)
  return function()
    hs.application.launchOrFocus(name)
  end
end

hs.hotkey.bind(fn, "1", focusApp("Obsidian"))
hs.hotkey.bind(fn, "2", focusApp("Firefox"))
hs.hotkey.bind(fn, "3", focusApp("Ghostty"))
hs.hotkey.bind(fn, "9", focusApp("Spotify"))

--------------------------------------------------------------------------------
-- FRAME HISTORY
--------------------------------------------------------------------------------

local frameHistory = {}

function recordFrame(window)
  local window = window or hs.window.focusedWindow()
  if not frameHistory[window:id()] then
    frameHistory[window:id()] = {}
  end
  local frame = window:frame()
  local history = frameHistory[window:id()]
  if history[#history] ~= frame then
    table.insert(frameHistory[window:id()], frame)
  end
end

function revertFrame(window)
  local window = window or hs.window.focusedWindow()
  local history = frameHistory[window:id()]
  if next(history) ~= nil then
    window:setFrame(history[#history])
    table.remove(history)
  end
end

hs.hotkey.bind(fn, "z", revertFrame, nil, revertFrame)

--------------------------------------------------------------------------------
-- MOVE & RESIZE
--------------------------------------------------------------------------------

-- Center
hs.hotkey.bind(fn, "c", function()
  recordFrame()
  local window = hs.window.focusedWindow()
  window:centerOnScreen()
end)

-- Maximize
hs.hotkey.bind(fn, "f", function()
  local window = hs.window.focusedWindow()
  local windowFrame = window:frame()
  local screenFrame = window:screen():frame()

  -- I hate imperative programming
  local copyFrame = function(frame)
    local x = frame.x
    local y = frame.y
    local w = frame.w
    local h = frame.h
    return hs.geometry.rect(x, y, w, h)
  end

  local pragmatic = copyFrame(screenFrame)
  -- local aestheticSize = function(ratio)
  --   local w = ratio
  --   local h = (screenFrame.h - (screenFrame.w * (1 - ratio))) / screenFrame.h
  --   return hs.geometry.size(w, h)
  -- end
  -- local aesthetic = copyFrame(pragmatic):scale(aestheticSize(0.95))

  if windowFrame:equals(pragmatic) then
  --   window:setFrame(aesthetic)
  -- elseif windowFrame:equals(aesthetic) then
  --   window:setFrame(pragmatic)
  else
    recordFrame()
    window:setFrame(pragmatic)
  end
end)

-- Left
hs.hotkey.bind(fn, "[", function()
  local window = hs.window.focusedWindow()
  local windowFrame = window:frame()
  local screenFrame = window:screen():frame()

  local buildFrame = function(scaleW, scaleH)
    local x = screenFrame.x
    local y = screenFrame.y
    local w = screenFrame.w * scaleW
    local h = screenFrame.h * scaleH
    return hs.geometry.rect(x, y, w, h)
  end

  local third = buildFrame(1 / 3, 1)
  local half = buildFrame(1 / 2, 1)
  local twoThirds = buildFrame(2 / 3, 1)

  if windowFrame:equals(half) then
    window:setFrame(twoThirds)
  elseif windowFrame:equals(twoThirds) then
    window:setFrame(third)
  elseif windowFrame:equals(third) then
    window:setFrame(half)
  else
    recordFrame()
    window:setFrame(half)
  end
end)

-- Right
hs.hotkey.bind(fn, "]", function()
  local window = hs.window.focusedWindow()
  local windowFrame = window:frame()
  local screenFrame = window:screen():frame()

  local buildFrame = function(scaleW, scaleH)
    local x = screenFrame.x + (screenFrame.w - (screenFrame.w * scaleW))
    local y = screenFrame.y + (screenFrame.h - (screenFrame.h * scaleH))
    local w = screenFrame.w * scaleW
    local h = screenFrame.h * scaleH
    return hs.geometry.rect(x, y, w, h)
  end

  local third = buildFrame(1 / 3, 1)
  local half = buildFrame(1 / 2, 1)
  local twoThirds = buildFrame(2 / 3, 1)

  if windowFrame == half then
    window:setFrame(twoThirds)
  elseif windowFrame == twoThirds then
    window:setFrame(third)
  elseif windowFrame == third then
    window:setFrame(half)
  else
    recordFrame()
    window:setFrame(half)
  end
end)

--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------

-- Reload config
hs.hotkey.bind(fn, "r", hs.reload)

hs.alert.show("Hammerspoon loaded!")
