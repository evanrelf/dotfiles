function ghc-shell
    nix-shell --log-format bar --expr "
let
    pkgs = import <nixpkgs> { };

    ghciwatch-compat =
        let
            flake = builtins.getFlake \"github:evanrelf/ghciwatch-compat\";
        in
            flake.packages.\${builtins.currentSystem}.ghciwatch-compat-ghcid;
in
pkgs.mkShell {
    packages = [
        (pkgs.ghc.withPackages (p: with p; [ $argv ]))
        ghciwatch-compat
    ];
}
"
end
