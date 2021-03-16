-- TODO: Use hs.window.filter

hs.window.animationDuration = 0

local fn = {"ctrl", "alt", "cmd"}

--------------------------------------------------------------------------------
-- FOCUS
--------------------------------------------------------------------------------

-- Focus left
hs.hotkey.bind(fn, "h", function()
  local window = hs.window.focusedWindow()
  local westWindows = window:windowsToWest(nil, true, false)
  if next(westWindows) == nil then
    local eastWindows = window:windowsToEast(nil, true, false)
    local eastmostWindow = eastWindows[#eastWindows]
    eastmostWindow:focus()
  else
    window:focusWindowWest()
  end
end)

-- Focus down
hs.hotkey.bind(fn, "j", function()
  local window = hs.window.focusedWindow()
  local southWindows = window:windowsToSouth(nil, true, false)
  if next(southWindows) == nil then
    local northWindows = window:windowsToNorth(nil, true, false)
    local northmostWindow = northWindows[#northWindows]
    northmostWindow:focus()
  else
    window:focusWindowSouth()
  end
end)

-- Focus up
hs.hotkey.bind(fn, "k", function()
  local window = hs.window.focusedWindow()
  local northWindows = window:windowsToNorth(nil, true, false)
  if next(northWindows) == nil then
    local southWindows = window:windowsToSouth(nil, true, false)
    local southmostWindow = southWindows[#southWindows]
    southmostWindow:focus()
  else
    window:focusWindowNorth()
  end
end)

-- Focus right
hs.hotkey.bind(fn, "l", function()
  local window = hs.window.focusedWindow()
  local eastWindows = window:windowsToEast(nil, true, false)
  if next(eastWindows) == nil then
    local westWindows = window:windowsToWest(nil, true, false)
    local westmostWindow = westWindows[#westWindows]
    westmostWindow:focus()
  else
    window:focusWindowEast()
  end
end)

--------------------------------------------------------------------------------
-- MOVE & RESIZE
--------------------------------------------------------------------------------

-- Center
hs.hotkey.bind(fn, "c", function()
  local window = hs.window.focusedWindow()
  window:centerOnScreen()
end)

-- Maximize
hs.hotkey.bind(fn, "f", function()
  local window = hs.window.focusedWindow()
  if window:isMaximizable() then
    window:maximize()
  else
    hs.alert.show("Window cannot be maximized")
  end
end)

-- Left 1/2
hs.hotkey.bind(fn, "[", function()
  local window = hs.window.focusedWindow()
  window:moveToUnit(hs.geometry.rect(0, 0, 0.5, 1))
end)

-- Right 1/2
hs.hotkey.bind(fn, "]", function()
  local window = hs.window.focusedWindow()
  window:moveToUnit(hs.geometry.rect(0.5, 0, 0.5, 1))
end)

--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------

-- Reload config
hs.hotkey.bind(fn, "r", hs.reload)

hs.alert.show("Hammerspoon loaded!")
