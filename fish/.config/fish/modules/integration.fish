# nnn
# if _exists nnn
#     function fish_greeting
#         if test \( -z "$NNNLVL" \) -a \( -n "$TMUX" \)
#             exec nnn
#         end
#     end
# end

# hg-root
# if ! _exists hg-root && _exists hg 2>&1
#     _warn "Your prompt may be slow if you don't have hg-root installed\nhttps://github.com/evanrelf/hg-root"
# end

# direnv
if _exists direnv
    set -x DIRENV_LOG_FORMAT ""
    eval (direnv hook fish)
end

# Add auto-completion for stack
function _stack
    set -l cl (commandline --tokenize --current-process)
    # Hack around fish issue #3934
    set -l cn (commandline --tokenize --cut-at-cursor --current-process)
    set -l cn (count $cn)
    set -l tmpline --bash-completion-enriched --bash-completion-index $cn
    for arg in $cl
        set tmpline $tmpline --bash-completion-word $arg
    end
    for opt in (stack $tmpline)
        if test -d $opt
            echo -E "$opt/"
        else
            echo -E "$opt"
        end
    end
end
complete --no-files --command stack --arguments '(_stack)'

# Nix
if test -e $HOME/.nix-profile/etc/profile.d/nix.sh
    if type -q bass
        bass source $HOME/.nix-profile/etc/profile.d/nix.sh
    else
        _warn "Nix (single user) isn't working because you don't have bass"
    end
end
if test -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    if type -q bass
        bass source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    else
        _warn "Nix (multi-user) isn't working because you don't have bass"
    end
end
# if _exists any-nix-shell
#     # any-nix-shell fish --info-right | source
#     any-nix-shell fish | source
# end

# home-manager
if test -e $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
    if type -q bass
        bass source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
    else
        _warn "home-manager isn't working because you don't have bass"
    end
end

# macOS
if test (uname) = "Darwin"
    if status --is-interactive
      abbr --add cask "brew cask"
    end

    function refresh -d "Restart system applications"
        # _silently defaults write com.apple.dock ResetLaunchPad -bool true
        # or _warn "Failed to reset Launchpad"

        _silently killall SystemUIServer
        or _warn "Failed to kill SystemUIServer"

        _silently killall Dock
        or _warn "Failed to kill Dock"

        _silently killall Finder
        or _warn "Failed to kill Finder"

        _silently killall ControlStrip
        or _warn "Failed to kill ControlStrip"
    end

    function rmds -d "Remove .DS_Store files recursively from current directory"
        if _exists fd
            _log "Searching..."
            set -l files (fd --hidden --no-ignore --case-sensitive --absolute-path --exclude '.Trash' .DS_Store)
            if test (count $files) -gt 0
                for i in $files
                    rm "$i"
                    and _log "Removed $i"
                    or _warn "* Failed to remove $i"
                end
            else
                _error "No .DS_Store files found"
                return 1
            end
        else
            _error "fd not installed"
            return 1
        end
    end

    function iso2img -d "Convert an ISO to an IMG"
        if test (count $argv) -gt 0
            for iso in $argv
                hdiutil convert -format UDRW -o "$iso.img" "$iso"
                and mv "$iso.img.dmg" "$iso.img"
                and mv "$iso.img" (echo "$iso.img" | sed "s/\.iso//")
            end
        else
            _error "No ISO files specified"
            return 1
        end
    end
    complete --command iso2img --require-parameter

    if test -e "/Applications/Marked 2.app"
        alias marked "open -a 'Marked 2.app'"
    end

    if test -e /usr/local/share/autojump/autojump.fish
        source /usr/local/share/autojump/autojump.fish
    end
end

# Arch
if uname -a | grep -q ARCH
    if test -e /usr/share/autojump/autojump.fish
        source /usr/share/autojump/autojump.fish
    end
end
