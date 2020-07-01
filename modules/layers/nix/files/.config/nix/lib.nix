{
  # Not using pkgs.fetchFromGitHub because it depends on Nixpkgs, which means
  # changing pinned versions rebuilds otherwise unchanged packages.
  fetchGitHub = { owner, repo, rev, sha256 }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    };
}
