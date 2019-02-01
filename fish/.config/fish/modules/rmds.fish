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
        end
    else
        _error "fd not installed"
    end
end
