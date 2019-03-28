if test (uname) = "Darwin"; and status --is-interactive
  abbr --add cask "brew cask"
end

function refresh -d "Restart system applications"
    # _silent defaults write com.apple.dock ResetLaunchPad -bool true
    # or _warn "Failed to reset Launchpad"

    _silent killall SystemUIServer
    or _warn "Failed to kill SystemUIServer"

    _silent killall Dock
    or _warn "Failed to kill Dock"

    _silent killall Finder
    or _warn "Failed to kill Finder"

    _silent killall ControlStrip
    or _warn "Failed to kill ControlStrip"
end

if _exists gittower
    alias tower "gittower ."
end

if test -e "/Applications/Marked 2.app"
    alias marked "open -a 'Marked 2.app'"
end

if test -f /usr/local/share/autojump/autojump.fish
    source /usr/local/share/autojump/autojump.fish
end

