hs.alert.show("Hammerspoon loaded!")

-- hs.window.animationDuration = 0.05
hs.window.animationDuration = 0
local hyper = {"shift", "cmd", "alt", "ctrl"}

local undoHistory = {}

hs.hotkey.bind(hyper, "\\", hs.reload)

local function appEvents()
  hs.application.watcher.new(function(appName, eventType, app)
    -- Auto-fullscreen Spotify on launch
    if appName == "Spotify" and eventType == hs.application.watcher.launched then
      local window = app:mainWindow()
      hs.timer.doAfter(2, function()
        window:setFullScreen(not window:isFullScreen())
      end)
    end

    -- -- Auto-maximize Affinity Photo on launch
    -- if appName == "Affinity Photo" and eventType == hs.application.watcher.launched then
    --   local window = app:mainWindow()
    --   hs.timer.doAfter(1, function()
    --     window:maximize()
    --   end)
    -- end

  end):start()
end

local function pushUndo()
  local windowState = hs.window.frontmostWindow():frame()
  -- Don't save consecutive duplicates
  if windowState ~= undoHistory[#undoHistory] then
    undoHistory[#undoHistory + 1] = windowState
  end
end

local function popUndo()
  if #undoHistory > 0 then
    local undo = undoHistory[#undoHistory]
    undoHistory[#undoHistory] = nil
    return undo
  else
    -- No more undos
    return nil
  end
end

local function wm()
  local window = hs.window.focusedWindow

  local function w(modifiers, key, pos)
    if not pos.x then pos.x = 0 end
    if not pos.y then pos.y = 0 end
    if not pos.w then pos.w = 1 end
    if not pos.h then pos.h = 1 end

    hs.hotkey.bind(modifiers, key, function()
      pushUndo()
      window():moveToUnit(hs.geometry.rect(pos.x, pos.y, pos.w, pos.h))
    end)
  end

  -- Top left 1/4
  w(hyper, "Q", { x = 0, y = 0, w = (1/2), h = (1/2) })
  -- Top right 1/4
  w(hyper, "W", { x = (1/2), y = 0, w = (1/2), h = (1/2) })
  -- Bottom left 1/4
  w(hyper, "A", { x = 0, y = (1/2), w = (1/2), h = (1/2) })
  -- Bottom right 1/4
  w(hyper, "S", { x = (1/2), y = (1/2), w = (1/2), h = (1/2) })

  -- Left 1/2
  w(hyper, "R", { x = 0, w = (1/2) })
  -- Right 1/2
  w(hyper, "T", { x = (1/2), w = (1/2) })

  -- Left 1/3
  w(hyper, "1", { x = 0, w = (1/3) })
  -- Middle 1/3
  w(hyper, "2", { x = (1/3), w = (1/3) })
  -- Right 1/3
  w(hyper, "3", { x = (2/3), w = (1/3) })

  -- Left 2/3
  w(hyper, "4", { x = 0, w = (2/3) })
  -- Middle 2/3
  w(hyper, "B", { x = (1/6), w = (2/3) })
  -- Right 2/3
  w(hyper, "5", { x = (1/3), w = (2/3) })

  -- Maximize
  w(hyper, "F", { w = 1, h = 1 })

  -- Center
  hs.hotkey.bind(hyper, "C", function()
    pushUndo()
    window():centerOnScreen()
  end)

  -- Undo
  hs.hotkey.bind(hyper, "Z", function()
    local undo = popUndo()
    if not undo then
      hs.alert.show("No undo history")
    else
      window():setFrame(undo)
    end
  end)
end

appEvents()
wm()
