{ pkgs, ... }:

{
  home.packages = with pkgs; [
    delta
    gh
    git-branchless
    gitAndTools.gitFull
    perlPackages.GitAutofixup
  ];

  xdg.configFile."git" = {
    source = ../../configs/git/.config/git;
    recursive = true;
  };
}
