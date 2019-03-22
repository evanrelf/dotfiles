set -x NIXPKGS_ALLOW_UNFREE 1

function os
    switch $argv[1]
        case install i add
            nix-env -iA (for package in $argv[2..-1]; echo nixpkgs.$package; end)

        case install-unstable iu add-unstable
            nix-env -iA (for package in $argv[2..-1]; echo nixpkgs-unstable.$package; end)

        case remove rm r
            nix-env -e $argv[2..-1]

        case update up u
            nix-env -u $argv[2..-1]

        case search s
            nix search $argv[2..-1]

        case list ls l
            nix-env -q
    end
end
complete --command os --require-parameter --no-files --arguments "install remove update search list"
