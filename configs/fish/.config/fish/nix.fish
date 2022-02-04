function __nix_source_configs
    set --local multi_user_path "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
    set --local single_user_path "$HOME/.nix-profile/etc/profile.d/nix.sh"
    if test -f "$multi_user_path"
        # "Cached" version to speed up startup time
        set --export --path NIX_PATH "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs:/nix/var/nix/profiles/per-user/root/channels"
        set --export NIX_PROFILES "/nix/var/nix/profiles/default /Users/$USER/.nix-profile"
        set --export NIX_SSL_CERT_FILE "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
        set --export NIX_USER_PROFILE_DIR "/nix/var/nix/profiles/per-user/$USER"
        set --export --prepend PATH "/nix/var/nix/profiles/default/bin"
        set --export --prepend PATH "/Users/$USER/.nix-profile/bin"
        # if _exists bass
        #     bass source "$multi_user_path"
        # else
        #     _warn "Nix isn't working because you don't have bass installed"
        # end
    else if test -f "$single_user_path"
        if _exists bass
            bass source "$single_user_path"
        else
            _warn "Nix isn't working because you don't have bass installed"
        end
    else
        _warn "Unable to find Nix shell config"
    end
end

if test -d "/nix"
    # This isn't necessary on NixOS
    if not _exists nixos-version
        __nix_source_configs
    end
    set --export NIXPKGS_ALLOW_UNFREE 1
    if test -d "$HOME/.nix-profile/channels/"
        set --export --prepend --path NIX_PATH "$HOME/.nix-profile/channels"
        set --export --prepend --path NIX_PATH "nixpkgs=$HOME/.nix-profile/channels/nixpkgs"
    else
        _warn "Using imperative Nix channels"
        set --export --prepend --path NIX_PATH "$HOME/.nix-defexpr/channels"
        set --export --prepend --path NIX_PATH "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
    end
end

if _exists nix
    function run
        if test ! -f "shell.nix" && test ! -f "default.nix"
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

    set -g with_packages ""
    set -g with_command ""

    function __with
        if test -z "$argv"
            _error "with what?"
            return 1
        end
        set with_packages ""
        set with_command ""
        for arg in $argv
            if test "$arg" = "run"
                set with_command "--run '"
                continue
            end
            if test -z "$with_command"
                set with_packages "$with_packages $arg"
            else
                set with_command "$with_command $arg"
            end
        end
        if test -n "$with_command"
            set with_command "$with_command'"
        end
    end

    function with
        __with $argv || return 1
        eval "nix-shell --packages $with_packages $with_command"
    end

    function withghc
        __with $argv || return 1
        eval "nix-shell --packages 'haskellPackages.ghcWithPackages (p: with p; [ $with_packages ])' $with_command"
    end

    function nix-show-drv
        nix show-derivation $argv[1] | jq . --color-output | less -R
    end

    alias nix-instantiate "nix-instantiate --no-gc-warning"
    alias nix-stray-roots "nix-store --gc --print-roots | grep --invert-match --extended-regexp '^(/nix/var/|\{censored|\{lsof)'"

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
        set PATH
        for p in "$nix_paths"
            set --append PATH "$p"
        end
        for p in "$non_nix_paths"
            set --append PATH "$p"
        end
    end

end
