pkgsFinal: pkgsPrev:

{
  iosevka-bin = pkgsPrev.iosevka-bin.override { variant = "ss08"; };

  nerdfonts = pkgsPrev.nerdfonts.override { fonts = [ "Iosevka" ]; };
}
