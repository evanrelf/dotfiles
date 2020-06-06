if test (uname) = "Darwin"
    function rmds -d "Remove .DS_Store files recursively from current directory"
        if _exists fd
            _log "Searching..."
            set --local files (fd --hidden --no-ignore --case-sensitive --absolute-path --exclude '.Trash' .DS_Store)
            if test (count $files) -gt 0
                for i in $files
                    rm "$i"
                    and _log "Removed '$i'"
                    or _warn "* Failed to remove '$i'"
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
        if test (count "$argv") -gt 0
            for iso in "$argv"
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
end
