require("vis")

vis.events.subscribe(vis.events.INIT, function()
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
  vis:command("set numbers")
  vis:command("set colorcolumn 81")
  vis:command("set autoindent on")
  vis:command("set tabwidth 2")
  vis:command("set expandtab on")
  vis:command("set show-tabs on")
  vis:command("set show-eof on")
  -- vis:command("set theme default-16")
end)
