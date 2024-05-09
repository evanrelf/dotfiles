final: prev:

{
  hello-roc =
    # TODO: How should dependencies be downloaded? Fixed output derivation
    # running `roc check` and dumping cached dependencies into `$out`, for the
    # final derivation to consume? `crane` for Rust could be a good reference.
    final.stdenv.mkDerivation {
      name = "hello-roc";
      src = ../src/roc/hello;
      nativeBuildInputs = [ final.roc ];
      buildPhase = ''
        mkdir -p $out/bin/
        roc build --optimize --output $out/bin/hello
      '';
    };
}
