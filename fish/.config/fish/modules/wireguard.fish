function wig -d "WireGuard"
    set -l interface "mullvad-us2"

    switch $argv[1]
        case u up start
            sudo wg-quick up $interface
        case d down stop
            sudo wg-quick down $interface
        case s status
            _silently wg show
            and _log "Down"
            or _log "Up"
        case "*"
            _error "Invalid command '$argv[1]'\nComands: up down status"
    end
end
