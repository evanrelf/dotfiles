function update -d "Run all update commands"
    switch (uname)
        case Darwin
            _log "Updating macOS software"
            softwareupdate -lia

            if _exists mas
                _log "Updating Mac App Store apps"
                mas upgrade
            end

            if _exists brew
                _log "Updating Homebrew formulae"
                brew update
                brew upgrade

                _log "Updating Homebrew casks"
                brew upgrade --cask
            end
    end

    if _exists tldr
        _log "Updating tldr"
        tldr --update
    end

    _log "Updating Fish command completions"
    fish_update_completions
end
