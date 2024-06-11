function wcore
    set --local file $argv[1]
    set --local ghc_args -O -fforce-recomp -ddump-simpl -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dno-typeable-binds -dsuppress-idinfo -no-keep-hi-files -no-keep-o-files
    watchexec --watch $file --restart -- ghc $ghc_args $file \| less
end
