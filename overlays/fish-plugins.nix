final: prev:

let
  fish-colored-man =
    final.fishPlugins.buildFishPlugin {
      pname = "fish-colored-man";
      version = "0-unstable-2021-07-15";
      src = final.fetchFromGitHub {
        owner = "decors";
        repo = "fish-colored-man";
        rev = "1ad8fff696d48c8bf173aa98f9dff39d7916de0e";
        hash = "sha256-uoZ4eSFbZlsRfISIkJQp24qPUNqxeD0JbRb/gVdRYlA=";
      };
    };

in
{
  evanrelf-fish =
    final.wrapFish {
      pluginPkgs = with final.fishPlugins; [
        fish-colored-man
        fzf
      ];
    };
}
