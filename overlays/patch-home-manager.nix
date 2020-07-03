self: super:

{
  home-manager =
    let
      originalSource =
        super.fetchFromGitHub {
          name = "home-manager-source";
          owner = "rycee";
          repo = "home-manager";
          rev = "7f7348b47049e8d25fb5b98db1d6215f8f643f0d";
          sha256 = "1s8j8h42k354xzwlqizbzxlqvsagx7yrxl20qfxgxmx522ajw63p";
        };
      patchedSource =
        super.stdenv.mkDerivation {
          name = "home-manager-source-patched";
          src = originalSource;
          phases = [ "unpackPhase" "patchPhase" "installPhase" ];
          installPhase = "cp -r . $out";
          patches = [
            ../patches/home-manager-kitty.patch
            ../patches/home-manager-tmux.patch
          ];
        };
    in
      super.home-manager.overrideAttrs (old: rec {
        version = "HEAD";
        src = patchedSource;
        # I have to redefine the `installPhase` using my new `src`, because the
        # old `installPhase` has the old `src` baked in, in a way that isn't
        # affected by `overrideAttrs`.
        installPhase = ''
          install -v -D -m755 ${src}/home-manager/home-manager $out/bin/home-manager
          substituteInPlace $out/bin/home-manager \
            --subst-var-by bash "${self.bash}" \
            --subst-var-by coreutils "${self.coreutils}" \
            --subst-var-by findutils "${self.findutils}" \
            --subst-var-by gnused "${self.gnused}" \
            --subst-var-by less "${self.less}" \
            --subst-var-by HOME_MANAGER_PATH '${src}'
          install -D -m755 home-manager/completion.bash \
            "$out/share/bash-completion/completions/home-manager"
        '';
      });
}
