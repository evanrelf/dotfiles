-- vim: foldenable foldmethod=marker
-- luacheck: globals hs

local module = {}

function module.shell(command)
  return function()
    return hs.execute(command)
  end
end

function module.appleScript(source)
  return function()
    return hs.osascript.applescript(source)
  end
end

function module.application(name)
  return module.shell("open -a " .. name)
end

module.iterm = module.appleScript([[
tell application "iTerm"
  create window with default profile
end tell
]])

function module.kitty()
  hs.application.launchOrFocus("kitty")
  hs.eventtap.keyStroke({"cmd"}, "n")
end

module.alacritty = module.shell("open -na Alacritty")

return module
