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
        final.coreutils
        final.delta
        final.findutils
        final.fzf
        final.git
        final.ncurses
      ];
    };
}
