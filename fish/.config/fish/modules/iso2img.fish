function iso2img -d "Convert an ISO to an IMG"
    if test (count $argv) -gt 0
        for iso in $argv
            hdiutil convert -format UDRW -o "$iso.img" "$iso"
            and mv "$iso.img.dmg" "$iso.img"
            and mv "$iso.img" (echo "$iso.img" | sed "s/\.iso//")
        end
    else
        _error "No ISO files specified"
    end
end
complete --command iso2img --require-parameter
