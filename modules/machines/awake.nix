{ ... }:

{
  xdg.configFile."git/local".text = ''
    [user]
      email = "evan@awakesecurity.com"
      signingkey = "A9434F37F71F5BCA"
  '';
}
