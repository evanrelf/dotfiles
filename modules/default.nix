let
  listDir = dir:
    builtins.map
      (path: dir + "/${path}")
      (builtins.attrNames (builtins.readDir dir));

in
builtins.concatMap listDir [
  ./programs
]
