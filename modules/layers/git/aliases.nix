{
  # ADD
  a = "add";
  aa = ''"!git add --all $@ && git status --short #"'';
  ap = "add --patch";

  # BRANCH
  b = "branch --verbose";
  bd = "branch --delete";
  bdf = "branch --delete --force";

  # CHECKOUT
  co = "checkout";
  cop = "checkout --patch";

  # CLEAN
  clean-ignored = "clean -Xdf";
  clean-untracked = "clean -xdi";

  # COMMIT
  c = "commit";
  amend = "commit --amend --no-edit -S";
  rename = ''"!if [ -z \"$(git status --porcelain)\" ]; then git commit --amend; else echo 'Refusing to rename while dirty' >&2; exit 1; fi #"'';
  undo-commit = "reset --soft HEAD~";

  # DIFF
  d = "diff";
  ds = "diff --staged";

  # FETCH
  f = "fetch";
  ff = "fetch --force";

  # LOG
  l = "lll --max-count=10";
  ll = "lll --max-count=100";
  lll = "log --graph --pretty=format:'%Cred%h%Creset %s%C(yellow)%d%Creset %Cgreen(%cr)%Creset %Cblue<%an>%Creset'";
  signed = "log --graph --max-count=10 --pretty=format:'%Cred%h%Creset %s%C(yellow)%d%Creset %Cgreen(%cr)%Creset %Cblue<%an>%Creset %C(magenta)%GS%Creset'";

  # LOOKUP (script)
  k = "lookup";

  # PULL
  pullff = "pull --ff-only";
  pullrb = "pull --rebase";

  # PUSH
  p = ''"!bash $HOME/.config/git/hooks/pre-push && git push $@ #"'';
  pf = "p --force-with-lease";

  # REBASE
  rb = "rebase";
  rbi = ''"!if [ -z \"$1\" ]; then git lookup | xargs -o git rebase-back; else git rebase-back $@; fi #"''; # script
  undo-rebase = "reset --hard ORIG_HEAD";
  cont = "rebase --continue";
  skip = "rebase --skip";
  abort = "rebase --abort";

  # RESET
  r = "reset";
  rp = "reset --patch";
  rs = "reset --soft";
  rh = "reset --hard";

  # REVISE
  rv = ''"!if [ -z \"$1\" ]; then git lookup | xargs git revise; else git revise $@; fi; echo \"Don't forget to resign commits with git sign\" #"'';
  sign = ''"!if [ -z \"$(git status --porcelain)\" ]; then git commit --amend --no-edit --gpg-sign; else echo 'Refusing to sign while dirty' >&2; exit 1; fi #"'';

  # SHOW
  sh = "shd --name-only";
  shd = ''"!if [ -z \"$1\" ]; then git lookup | xargs git show; else git show $@; fi #"'';

  # SPARSE-CHECKOUT
  sc = "sparse-checkout";
  sce = ''"!file=\"$(git root)/.git/info/sparse-checkout\"; if [ -f \"$file\" ]; then $EDITOR $file; else echo 'Sparse checkout file does not exist' >&2; exit 1; fi #"'';

  # STASH
  st = "stash push";
  stu = "stash push --include-untracked";
  sts = "stash show";
  sta = "stash apply";
  stp = "stash pop";
  stl = "stash list";
  std = "stash drop";
  # stdd = "stash clear";

  # STATUS
  s = "status --show-stash";
  ss = "status --short";

  # TAG
  t = "tag";
  td = "tag --delete";

  # WORKTREE
  wt = "worktree";

  # OTHER
  # Print the path to the root of the git repo
  root = "rev-parse --show-toplevel";
  # Print the commit hash
  hash = "rev-parse --default HEAD";
  # Print current branch
  current-branch = ''"!git symbolic-ref --short HEAD --quiet || git branch | head -n 1 | awk '{print $NF}' | tr -d ')' #"'';
  # Run a command from the root of the git repo
  exec = ''"!exec "'';
  blank = ''"!if [ -z \"$1\" ]; then exit 1; else git checkout --orphan $1 && git rm --cached -r .; fi #"'';
}
