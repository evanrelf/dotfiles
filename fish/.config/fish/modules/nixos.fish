function os
    switch $argv[1]
        case install i
            nix-env -i $argv[2..-1]

        case remove r
            nix-env -e $argv[2..-1]

        case update u
            nix-env -u $argv[2..-1]

        case search s
            nix search $argv[2..-1]

        case list l
            nix-env -q
    end
end
complete --command os --require-parameter --no-files --arguments "install remove update search list"
