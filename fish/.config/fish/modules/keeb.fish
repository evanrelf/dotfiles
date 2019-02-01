function keeb -d "Safely flash QMK firmware to TADA68 keyboard"
    set -l qmk_dir "$HOME/Projects/qmk_firmware"
    set -l mount_dir "/Volumes/TADA68  "

    if test ! -d "$mount_dir"
        _error "Volume not mounted at '$mount_dir'"
    end

    if test ! -d "$qmk_dir"
        _error "QMK directory '$qmk_dir' does not exist"
    else
        cd $qmk_dir; or _error "Failed to change directory to '$qmk_dir'"
    end

    if git remote -v | not grep "qmk_firmware" >/dev/null
        _error "Invalid QMK directory: '$qmk_dir'"
    end

    if not command -s nix-shell >/dev/null
        _error "Nix not installed"
    else
        nix-shell --arg arm false --command "make tada68:evanrelf"
        if test $status -ne 0
            _error "Failed to compile firmware"
        end
    end

    for file in (command ls -A "$mount_dir")
        command rm -rf "$mount_dir/$file"
        if test $status -ne 0
            _error "Failed to remove '$file' from volume"
        end
    end

    if test ! -e "tada68_evanrelf.bin"
        _error "Firmware file 'tada68_evanrelf.bin' does not exist"
    else
        cp "tada68_evanrelf.bin" "$mount_dir/FLASH.BIN"
        if test $status -ne 0
            _error "Failed to copy firmware to volume"
        end
    end

    set -l files (command ls -A "$mount_dir")

    if test $files = "FLASH.BIN"
        _log "SUCCESS"
        _warn "DO NOT EJECT DRIVE ON COMPUTER"
        _warn "PRESS ESCAPE KEY ON KEYBOARD TO FINISH"
    end

    cd -
end
