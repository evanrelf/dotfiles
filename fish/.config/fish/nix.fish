function nix_source_configs
    if _exists bass
        if set --local multi_user_path "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
            # bass source "$multi_user_path"
            # "Cached" version to speed up startup time
            set --export NIX_PATH "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs:/nix/var/nix/profiles/per-user/root/channels"
            set --export NIX_PROFILES "/nix/var/nix/profiles/default /Users/$USER/.nix-profile"
            set --export NIX_SSL_CERT_FILE "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
            set --export NIX_USER_PROFILE_DIR "/nix/var/nix/profiles/per-user/$USER"
            set --append PATH "/Users/$USER/.nix-profile/bin"
            set --append PATH "/nix/var/nix/profiles/default/bin"
        else if set --local single_user_path "$HOME/.nix-profile/etc/profile.d/nix.sh"
            bass source "$single_user_path"
        else
            _warn "Unable to find Nix shell config"
        end
    else
        _warn "Nix isn't working because you don't have bass installed"
    end
end

if test -d "/nix"
    set --export NIXPKGS_ALLOW_UNFREE 1
    set --append NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
    set --append NIX_PATH "$HOME/.nix-defexpr/channels"
    nix_source_configs
end

if _exists nix-shell
    function run
        if not test -f "shell.nix" && not test -f "default.nix"
            _error "Couldn't find 'shell.nix' or 'default.nix' in the current directory"
            return 1
        end
        _log "Entering Nix shell..."
        if test -z "$argv"
            nix-shell
        else
            nix-shell --command "$argv; return"
        end
    end

    function with
        if test -z "$argv"
            _error "with what?"
            return 1
        end
        _log "Entering Nix shell..."
        nix-shell --packages $argv
    end
end

if _exists nix-store
    alias nix-stray-roots "nix-store --gc --print-roots | grep --invert-match --extended-regexp '^(/nix/var/|\{censored|\{lsof)'"
end

if test -n "$IN_NIX_SHELL"
    set --local nix_paths
    set --local non_nix_paths
    for p in "$PATH"
        if test "$p" = "."
            true
        else if echo "$p" | grep --quiet "/nix/store"
            set --append nix_paths "$p"
        else
            set --append non_nix_paths "$p"
        end
    end
    set --export PATH
    for p in "$nix_paths"
        set --append PATH "$p"
    end
    for p in "$non_nix_paths"
        set --append PATH "$p"
    end
end
