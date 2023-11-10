final: prev:

{
  iosevka-bin = prev.iosevka-bin.override { variant = "ss08"; };

  nerdfonts = prev.nerdfonts.override { fonts = [ "Iosevka" ]; };
}
