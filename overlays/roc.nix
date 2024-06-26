final: prev:

let
  roc2nix =
    final.inputs.roc2nix.lib.${final.system}.overrideToolchain final.roc;

in
{
  # Broken
  hello-roc-roc2nix =
    roc2nix.buildRocApp {
      name = "hello-roc";
      src = ../src/roc/hello;
      rocDeps = {
        "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" = "sha256-PK+/rqUW52ic7U9VdTGFbrY19njKcwNa9vuRlI1QqK0=";
        "https://github.com/smores56/weaver/releases/download/0.2.0/BBDPvzgGrYp-AhIDw0qmwxT0pWZIQP_7KOrUrZfp_xw.tar.br" = "sha256-T0DRXY/HLhy+LHt/6jc5CAfK62gH4GUJGbmhLLAFcbM=";
      };
    };

  # Broken because `link_macos` seemingly hardcodes stuff assuming absolute
  # paths outside of the sandbox.
  #
  # ```
  # thread 'main' panicked at crates/compiler/build/src/program.rs:1032:30:
  # not yet implemented: gracefully handle `ld` failing to spawn.
  # ```
  #
  # https://github.com/roc-lang/roc/blob/674b9348215118a7c977d0685979d850d3a5f649/crates/compiler/build/src/link.rs#L1056
  hello-roc-manual =
    let
      dependencies =
        final.stdenv.mkDerivation {
          name = "hello-roc-dependencies";
          src = ../src/roc/hello;
          nativeBuildInputs = [ final.roc ];
          buildPhase = ''
            export HOME=$(mktemp -d)
            roc check
            mkdir $out
            mv $HOME/.cache/roc/packages $out/packages
          '';
          outputHashAlgo = "sha256";
          outputHashMode = "recursive";
          outputHash = "sha256-iEI4bAsOw/2FWRwXXNBnvbS9YulOEN/j7H31d4NP2tg=";
        };
    in
    final.stdenv.mkDerivation {
      name = "hello-roc";
      src = ../src/roc/hello;
      nativeBuildInputs = [
        dependencies
        final.roc
      ] ++ final.lib.optional final.stdenv.isDarwin [
        final.darwin.DarwinTools
        final.darwin.Libsystem
        final.darwin.apple_sdk.frameworks.AudioUnit
        final.darwin.apple_sdk.frameworks.Cocoa
        final.darwin.apple_sdk.frameworks.CoreAudio
        final.darwin.apple_sdk.frameworks.CoreFoundation
        final.darwin.apple_sdk.frameworks.CoreVideo
        final.darwin.apple_sdk.frameworks.IOKit
        final.darwin.apple_sdk.frameworks.Metal
        final.darwin.apple_sdk.frameworks.QuartzCore
        final.darwin.apple_sdk.frameworks.Security
        final.darwin.binutils
        final.darwin.libpthread
        final.darwin.libresolv
      ];
      buildPhase = ''
        export HOME=$(mktemp -d)
        mkdir -p $HOME/.cache/roc/
        ln -s ${dependencies}/packages $HOME/.cache/roc/packages
        mkdir -p $out/bin/
        roc build --optimize --output $out/bin/hello
      '';
      # __noChroot = true; # Requires `sandbox = relaxed`
      RUST_BACKTRACE = "1";
    };
}
