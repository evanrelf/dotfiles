if not _exists fisher
    _log "Installing fisher..."
    curl "https://raw.githubusercontent.com/jorgebucaran/fisher/master/fisher.fish" \
        --silent \
        --location \
        --create-dirs \
        --output "$HOME/.config/fish/functions/fisher.fish"
    source "$HOME/.config/fish/functions/fisher.fish"
    fisher update
end
