{
  user = {
    useConfigOnly = true;
  };

  core = {
    # "pager" = "diff-so-fancy | less --tabs=8 -RFX";
    pager = "delta --theme=GitHub --hunk-style=plain --highlight-removed --tabs=8 --file-color=yellow --file-style=box";
  };

  status = {
    showUntrackedFiles = "all";
  };

  stash = {
    showPatch = true;
    showStat = true;
  };

  commit = {
    template = "~/.config/git/template";
  };

  push = {
    default = "current";
    followTags = true;
  };

  diff = {
    renames = "copies";
    algorithm = "patience";
  };

  "diff \"nodiff\"" = {
    command = true;
  };

  merge = {
    # "conflictstyle" = "diff3";
  };

  color = {
    ui = true;
  };

  "color \"diff-highlight\"" = {
    oldNormal = "red";
    oldHighlight = "red 52";
    # "oldHighlight" = "red reverse";
    newNormal = "green";
    newHighlight = "green 22";
    # "newHighlight" = "green reverse";
  };

  "color \"diff\"" = {
    meta = "yellow";
    frag = "magenta";
    commit = "yellow";
    old = "red";
    new = "green";
    whitespace = "red reverse";
  };

  fetch = {
    prune = true;
    pruneTags = true;
  };

  interactive = {
    singleKey = true;
  };
}
