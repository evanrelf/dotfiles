-- TODO: Use hs.window.filter

hs.window.animationDuration = 0

local fn = { "ctrl", "alt", "cmd" }
local sfn = { "shift", "ctrl", "alt", "cmd" }

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
  recordFrame()
  local window = hs.window.focusedWindow()
  if window:isMaximizable() then
    window:maximize()
  else
    hs.alert.show("Window cannot be maximized")
  end
end)

-- Left 1/2
hs.hotkey.bind(fn, "[", function()
  recordFrame()
  local window = hs.window.focusedWindow()
  window:moveToUnit(hs.geometry.rect(0, 0, 1 / 2, 1))
end)

-- Right 1/2
hs.hotkey.bind(fn, "]", function()
  recordFrame()
  local window = hs.window.focusedWindow()
  window:moveToUnit(hs.geometry.rect(0.5, 0, 1 / 2, 1))
end)

-- Left 2/3
hs.hotkey.bind(sfn, "[", function()
  recordFrame()
  local window = hs.window.focusedWindow()
  window:moveToUnit(hs.geometry.rect(0, 0, 2 / 3, 1))
end)

-- Right 2/3
hs.hotkey.bind(sfn, "]", function()
  recordFrame()
  local window = hs.window.focusedWindow()
  window:moveToUnit(hs.geometry.rect(1 / 3, 0, 2 / 3, 1))
end)

--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------

-- Reload config
hs.hotkey.bind(fn, "r", hs.reload)

hs.alert.show("Hammerspoon loaded!")
