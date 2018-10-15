-- vim: foldenable foldmethod=marker
-- luacheck: globals hs

local module = {}

local undoStack = {}
local redoStack = {}
local saved = {}

local function push(history)
  local windowState = hs.window.frontmostWindow():frame()
  -- Don't save consecutive duplicates
  if windowState ~= history[#history] then
    history[#history + 1] = windowState
  end
end

local function pop(history)
  if #history == 0 then
    return nil
  end
  local state = history[#history]
  history[#history] = nil
  return state
end

local function log()
  push(undoStack)
  redoStack = {}
end

local function resize(pos)
  local win = hs.window.focusedWindow():frame()
  local screen = hs.screen.mainScreen():frame()
  if not pos.x then pos.x = 0 end
  if not pos.y then pos.y = 0 end
  if not pos.w then pos.w = 1 end
  if not pos.h then pos.h = 1 end
  local dw = math.abs(win.w - (pos.w * screen.w))
  local dh = math.abs(win.h - (pos.h * screen.h))
  local threshold = 10
  -- Allow animations if the window size doesn't change much
  if dw > threshold or dh > threshold then
    hs.window.animationDuration = 0
  else
    hs.window.animationDuration = 0.05
  end
  log()
  hs.window.focusedWindow():moveToUnit(hs.geometry.rect(pos.x, pos.y, pos.w, pos.h))
  hs.window.animationDuration = 0.2
end

function module.undo()
  local state = pop(undoStack)
  if not state then
    hs.alert("No undo history")
  else
    push(redoStack)
    hs.window.focusedWindow():setFrame(state)
  end
end

function module.redo()
  local state = pop(redoStack)
  if not state then
    hs.alert("No redo history")
  else
    push(undoStack)
    hs.window.focusedWindow():setFrame(state)
  end
end

function module.center()
  log()
  hs.window.focusedWindow():centerOnScreen()
end

function module.full()
  resize({})
end

function module.custom(pos)
  return function() resize(pos) end
end

-- 1/2 (horizontal) {{{
function module.halfLeft()
  resize({ x = 0, w = (1/2) })
end

function module.halfCenter()
  resize({ x = (1/4), w = (1/2) })
end

function module.halfRight()
  resize({ x = (1/2), w = (1/2) })
end
-- }}}

-- 1/2 (vertical) {{{
-- TODO
-- }}}

-- 1/3 {{{
function module.thirdLeft()
  resize({ x = 0, w = (1/3) })
end

function module.thirdCenter()
  resize({ x = (1/3), w = (1/3) })
end

function module.thirdRight()
  resize({ x = (2/3), w = (1/3) })
end
-- }}}

-- 2/3 {{{
function module.twoThirdsLeft()
  resize({ x = 0, w = (2/3) })
end

function module.twoThirdsCenter()
  resize({ x = (1/6), w = (2/3) })
end

function module.twoThirdsRight()
  resize({ x = (1/3), w = (2/3) })
end
-- }}}

-- 1/4 {{{
-- TODO
-- }}}

-- Saved {{{
function module.save()
  saved = hs.window.focusedWindow():frame()
  hs.alert("Saved window state")
end

function module.load()
  log()
  hs.window.focusedWindow():setFrame(saved)
  hs.alert("Loaded window state")
end
--- }}}

return module
