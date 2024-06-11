final: prev:

{
  evan = assert !(prev ? evan); {
    lib = {
      checkVersion = version: drv:
        let
          older = builtins.compareVersions (drv.version or "0") version < 0;
          error = builtins.throw ''
            '${drv.pname}' override is outdated

            Version from Nixpkgs:  ${drv.version}
            Version from dotfiles: ${version}
          '';
        in
        assert older || error; drv;
    };
  };
}
