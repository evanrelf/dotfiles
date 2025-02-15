final: prev:

{
  git-confirm-push =
    final.writeShellApplication {
      name = "git-confirm-push";
      text = builtins.readFile ../src/bash/git-confirm-push;
      runtimeInputs = [
        final.git
        final.ripgrep
        final.sd
      ];
    };

  git-lookup =
    final.writeShellApplication {
      name = "git-lookup";
      text = builtins.readFile ../src/bash/git-lookup;
      runtimeInputs = [
        final.delta
        final.findutils
        final.fzf
        final.gawk
        final.git
        final.ncurses
      ];
    };

  jj-lookup =
    final.writeShellApplication {
      name = "jj-lookup";
      text = builtins.readFile ../src/bash/jj-lookup;
      runtimeInputs = [
        final.delta
        final.findutils
        final.fzf
        final.gawk
        final.git
        final.jujutsu
        final.ncurses
      ];
    };
}
