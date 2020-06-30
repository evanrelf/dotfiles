-- vim: foldenable foldmethod=marker
-- luacheck: globals hs

local module = {}

local undoStack = {}
local redoStack = {}
local saved = {}

hs.window.animationDuration = 0

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
  if not pos.x then pos.x = 0 end
  if not pos.y then pos.y = 0 end
  if not pos.w then pos.w = 1 end
  if not pos.h then pos.h = 1 end
  log()
  hs.window.focusedWindow():moveToUnit(hs.geometry.rect(pos.x, pos.y, pos.w, pos.h))
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

function module.nextScreen()
  log()
  hs.window.focusedWindow():moveToScreen(hs.screen.mainScreen():next())
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

-- 1/2 {{{
function module.halfLeft()
  resize({ w = (1/2) })
end

function module.halfCenter()
  resize({ x = (1/4), w = (1/2) })
end

function module.halfRight()
  resize({ x = (1/2), w = (1/2) })
end

function module.halfTop()
  resize({ h = (1/2) })
end

function module.halfBottom()
  resize({ y = (1/2), h = (1/2) })
end
-- }}}

-- 1/3 {{{
function module.thirdLeft()
  resize({ w = (1/3) })
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
  resize({ w = (2/3) })
end

function module.twoThirdsCenter()
  resize({ x = (1/6), w = (2/3) })
end

function module.twoThirdsRight()
  resize({ x = (1/3), w = (2/3) })
end
-- }}}

-- 1/4 {{{
function module.quarterTopLeft()
  resize({ w = (1/2), h = (1/2) })
end

function module.quarterTopRight()
  resize({ x = (1/2), w = (1/2), h = (1/2) })
end

function module.quarterBottomLeft()
  resize({ y = (1/2), w = (1/2), h = (1/2) })
end

function module.quarterBottomRight()
  resize({ x = (1/2), y = (1/2), w = (1/2), h = (1/2) })
end
-- }}}

-- Saved {{{
function module.save()
  saved = hs.window.focusedWindow():size()
  hs.alert("Saved window size")
end

function module.load()
  log()
  hs.window.focusedWindow():setSize(saved)
  hs.alert("Loaded window size")
end
--- }}}

return module
