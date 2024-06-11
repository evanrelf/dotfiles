function wspago
    watchexec --exts purs,js,yaml --restart --clear --quiet -- spago build $argv
end
