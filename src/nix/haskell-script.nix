{ darwin
, ghc
, ghciwatch-compat-ghcid
, lib
, lndir
, makeWrapper
, mkShell
, runCommand
, stdenv
}:

let
  defaultGhcOptions =
    lib.concatStringsSep " " [
      "-Wall"
      "-Wcompat"
      "-Werror=incomplete-patterns"
      "-Werror=incomplete-record-updates"
      "-Werror=incomplete-uni-patterns"
      "-Werror=missing-fields"
      "-Werror=missing-methods"
      "-Werror=partial-fields"
      "-Widentities"
      "-Wmissing-export-lists"
      "-Wmissing-home-modules"
      "-Wno-unticked-promoted-constructors"
      "-Wredundant-constraints"
      "-O2"
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];

in

{ name
, packages ? _: [ ]
, substitutions ? { }
, path ? [ ]
, script
, ghcOptions ? "${defaultGhcOptions} ${extraGhcOptions}"
, extraGhcOptions ? ""
}:

let
  ghcWithPackages = ghc.withPackages packages;

  customGhc =
    runCommand
      "custom-ghc"
      { buildInputs = [ ghcWithPackages makeWrapper ]; }
      ''
        mkdir $out
        ${lndir}/bin/lndir -silent ${ghcWithPackages} $out
        rm $out/bin/ghc $out/bin/ghci
        makeWrapper ${ghcWithPackages}/bin/ghc $out/bin/ghc --add-flags "${ghcOptions}"
        makeWrapper ${ghcWithPackages}/bin/ghci $out/bin/ghci --add-flags "${ghcOptions}"
      '';

  substituteCommand =
    if builtins.attrNames substitutions == [ ] then
      "ln --symbolic ${script} Main.hs"
    else
      let
        toOption = name: value:
          let
            shellName = lib.escapeShellArg name;
            shellValue = lib.escapeShellArg (toString value);
          in
          "--subst-var-by ${shellName} ${shellValue}";

        options =
          lib.concatStringsSep " "
            (lib.mapAttrsToList toOption substitutions);
      in
      "substitute ${script} Main.hs ${options}";

  wrapCommand =
    if path == [ ]
    then ""
    else "wrapProgram $out/bin/${name} --prefix PATH : ${lib.makeBinPath path}";

in
runCommand
  name
{
  nativeBuildInputs = [ customGhc ]
    ++ lib.optional stdenv.isDarwin darwin.cctools
    ++ lib.optional (path != [ ]) makeWrapper;

  passthru.env =
    mkShell {
      packages = [
        customGhc
        ghciwatch-compat-ghcid
      ];
    };
}
  ''
    ${substituteCommand}
    mkdir --parents $out/bin
    ghc -Werror -package ghc Main.hs -o $out/bin/${name}
    ${wrapCommand}
  ''
