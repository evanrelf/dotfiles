{ config, inputs, ... }:

# Credit to @Xe: https://twitter.com/theprincessxena/status/1473446871272239104

{
  system.configurationRevision =
    inputs.self.sourceInfo.rev;

  services.getty.greetingLine = ''
    <<< Welcome to NixOS ${config.system.nixos.label} (\m) - \l >>>
    <<< dotfiles rev: ${inputs.self.sourceInfo.rev} >>>'';
}
